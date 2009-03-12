module Ratio =
  struct
    include Ratio

    let create_helper cast num denom = div_ratio (cast num) (cast denom)
    let create_ratio_int = create_helper ratio_of_int
    let create_ratio_string = create_helper ratio_of_string
  end

