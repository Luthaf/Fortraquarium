program fortraquarium
    use aquariums
    implicit none
    integer :: i
    type(aquarium) :: aqua

    aqua = aquarium()

    call aqua%add_fish("Marcel", 'M', 'F')
    call aqua%add_fish("Sonia", 'F', 'S')
    call aqua%add_fish("Georges", 'M', 'S')

    call aqua%add_seaweed
    call aqua%add_seaweed

    call aqua%live
end program
