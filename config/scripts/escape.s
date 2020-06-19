self = 4096

module_count      = 0
module_base       = 2
jump_module_index = 4

module_component = 0
module_part_count = 2
module_part_base = 4
module_control = 6
module_vector = 8
module_condition = 10
module_tec_level = 11

module_part_component = 0

jump_start_charge = 0
jump_engage = 1

escape:
    mov @#self, r0
    mov module_base(r0), r1
    mov jump_module_index(r0), r2
    asl r2
    add r2, r1
    mov (r1), r1
    mov #jump_ready, module_vector (
    mov #1, jump_start_charge (r1)
    