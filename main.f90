program fortraquarium
    use aquariums
    implicit none
    integer :: i
    type(aquarium) :: aqua

    aqua = aquarium()

    call aqua%add_fish("Marcel", .true.)
    call aqua%add_fish("Sonia", .false.)
    call aqua%add_fish("Georges", .true.)

    call aqua%add_seaweed
    call aqua%add_seaweed

    call aqua%live
end program
