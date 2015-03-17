module aquariums
    use fishes
    use seaweeds
    implicit none
    integer, parameter :: MAX_FISHES=100, MAX_SEAWEEDS=100

    type aquarium
        type(seaweed), dimension(MAX_FISHES) :: seaweeds
        type(fish), dimension(MAX_SEAWEEDS) :: fishes
        integer :: last_seaweed, last_fish, step
    contains
        procedure :: add_fish
        procedure :: add_seaweed
        procedure :: live
    end type

    interface aquarium
        procedure new_aquarium
    end interface

contains
    function new_aquarium()
        implicit none
        type(aquarium) :: new_aquarium
        new_aquarium%last_fish = 0
        new_aquarium%last_seaweed = 0
        new_aquarium%step = 0
    end function

    subroutine add_fish(this, name, is_male)
        implicit none
        class(aquarium), intent(inout) :: this
        character(len=*), intent(in) :: name
        logical, intent(in) :: is_male

        if (this%last_fish < MAX_FISHES) then
            this%last_fish = this%last_fish + 1
            this%fishes(this%last_fish) = fish(name, is_male)
        else
            write(*, *) "Error: too much fishes in aquarium"
        end if
    end subroutine

    subroutine add_seaweed(this)
        implicit none
        class(aquarium), intent(inout) :: this

        if (this%last_seaweed < MAX_SEAWEEDS) then
            this%last_seaweed = this%last_seaweed + 1
            this%seaweeds(this%last_seaweed) = seaweed()
        else
            write(*, *) "Error: too much seaweeds in aquarium"
        end if
    end subroutine

    subroutine live(this)
        implicit none
        class(aquarium), intent(inout) :: this

        call write(this)
    end subroutine

    subroutine write(aqua)
        implicit none
        class(aquarium), intent(in) :: aqua
        integer :: i

        write(*, *) "Aquarium at step ", aqua%step
        write(*, *) "We have ", aqua%last_seaweed, " seaweeds."
        write(*, *) "Fishes list:"
        do i=1,aqua%last_fish
            write(*, *) "    ", trim(aqua%fishes(i)%name), " is a ", aqua%fishes(i)%sex()
        end do
    end subroutine
end module
