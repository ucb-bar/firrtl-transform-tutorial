module SoftErrorFlopModel #(parameter WIDTH=1, parameter ERROR_PPM=10)
   (
    input                  clock,
    input [WIDTH-1:0]      D,
    output reg [WIDTH-1:0] Q
    );

   import "DPI-C" function int easy_random ();

   genvar                  i;
   generate
      always @(posedge clock) begin
         for (i = 0; i < WIDTH; i = i + 1) begin
            Q[i] <= D[i];
            if (({easy_random()} % 1000000) < ERROR_PPM)
              Q[i] <= ~D[i]; // Random bit flip
         end
      end
   endgenerate

endmodule // SoftErrorFlopModel

