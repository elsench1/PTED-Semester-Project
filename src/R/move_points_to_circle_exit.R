library(sf)
library(dplyr)

move_points_to_circle_exit <- function(GPS_Track, hide_point, dist) {
  
  # Safety check: both objects must use the same coordinate reference system (CRS).
  # If they differ, transform the hide point to match the CRS of the GPS track.
  if (st_crs(GPS_Track) != st_crs(hide_point)) {
    hide_point <- st_transform(hide_point, st_crs(GPS_Track))
  }
  
  # Get the center of the privacy circle.
  # This is the coordinate of the hide point.
  center <- as.numeric(st_coordinates(hide_point)[1, c("X", "Y")])
  
  # Extract the x/y coordinates of all GPS track points.
  coords <- st_coordinates(GPS_Track)[, c("X", "Y")]
  
  # Calculate the distance from each track point to the circle center.
  # Points with a distance smaller than or equal to `dist` are inside the privacy circle.
  dists <- sqrt((coords[, 1] - center[1])^2 + (coords[, 2] - center[2])^2)
  inside <- dists <= dist
  
  # Helper function: calculate where a line segment crosses the circle boundary.
  # `p_inside` is expected to be a point inside the circle.
  # `p_outside` is expected to be the next point outside the circle.
  # The returned point is the exit point on the circle boundary.
  get_exit_point <- function(p_inside, p_outside, center, radius) {
    # Direction vector of the segment from the inside point to the outside point.
    d <- p_outside - p_inside
    
    # Vector from the circle center to the inside point.
    f <- p_inside - center
    
    # Coefficients of the quadratic equation for the intersection
    # between the line segment and the circle.
    a <- sum(d * d)
    b <- 2 * sum(f * d)
    c <- sum(f * f) - radius^2
    
    # Discriminant of the quadratic equation.
    # If it is negative, the segment does not intersect the circle numerically.
    disc <- b^2 - 4 * a * c
    
    if (a == 0 || disc < 0) {
      # Fallback: project the inside point radially onto the circle boundary.
      # This handles duplicate points or numerical edge cases.
      v <- p_inside - center
      len <- sqrt(sum(v * v))
      
      # If the point lies exactly at the center, choose an arbitrary direction.
      if (len == 0) {
        return(center + c(radius, 0))
      }
      
      return(center + radius * v / len)
    }
    
    sqrt_disc <- sqrt(disc)
    
    # Solve the quadratic equation.
    # The parameter t describes positions along the segment:
    # t = 0 is p_inside, t = 1 is p_outside.
    t_candidates <- c(
      (-b - sqrt_disc) / (2 * a),
      (-b + sqrt_disc) / (2 * a)
    )
    
    # Keep only intersections that lie on the actual segment.
    t <- t_candidates[t_candidates >= 0 & t_candidates <= 1]
    
    if (length(t) == 0) {
      # Fallback in case no clean intersection is found due to numerical precision.
      # Again, project the inside point radially onto the circle boundary.
      v <- p_inside - center
      len <- sqrt(sum(v * v))
      
      if (len == 0) {
        return(center + c(radius, 0))
      }
      
      return(center + radius * v / len)
    }
    
    # Since the segment starts inside the circle and ends outside,
    # the larger valid t corresponds to the exit point.
    p_inside + max(t) * d
  }
  
  # Prepare a copy of the original coordinates.
  # Only points inside the privacy circle will be modified.
  new_coords <- coords
  
  # Find consecutive blocks of points that are inside the privacy circle.
  # rle() identifies runs of TRUE/FALSE values in the `inside` vector.
  r <- rle(inside)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  
  for (i in seq_along(r$values)) {
    # Skip blocks that are outside the privacy circle.
    if (!r$values[i]) next
    
    # Start and end index of the current block of private points.
    start_idx <- starts[i]
    end_idx <- ends[i]
    
    # Index of the first point after the private block.
    after_idx <- end_idx + 1
    
    if (after_idx <= nrow(coords)) {
      # Normal case: the track leaves the circle after this block.
      # Calculate the exact point where the segment exits the circle.
      exit_point <- get_exit_point(
        p_inside = coords[end_idx, ],
        p_outside = coords[after_idx, ],
        center = center,
        radius = dist
      )
    } else {
      # Edge case: the track ends while still inside the circle.
      # In this case, move the last inside point radially onto the circle boundary.
      v <- coords[end_idx, ] - center
      len <- sqrt(sum(v * v))
      
      # If the point is exactly at the center, use an arbitrary direction.
      if (len == 0) {
        exit_point <- center + c(dist, 0)
      } else {
        exit_point <- center + dist * v / len
      }
    }
    
    # Move all points in this private block to the calculated exit point.
    # This hides the detailed movement inside the privacy circle while keeping
    # the track connected to the point where it leaves the circle.
    new_coords[start_idx:end_idx, 1] <- exit_point[1]
    new_coords[start_idx:end_idx, 2] <- exit_point[2]
  }
  
  # Rebuild and return an sf object with the updated coordinates.
  # The original attribute columns are preserved, while the geometry is replaced.
  out <- GPS_Track |> 
    st_drop_geometry() |> 
    mutate(
      .x_new = new_coords[, 1],
      .y_new = new_coords[, 2]
    ) |> 
    st_as_sf(
      coords = c(".x_new", ".y_new"),
      crs = st_crs(GPS_Track)
    )
  
  return(out)
}