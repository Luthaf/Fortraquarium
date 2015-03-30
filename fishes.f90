module fishes
    use seaweeds
    implicit none
    private
    public :: fish, fishivorous, herbivorous

    type fish
        character(len=80) name
        logical male
    contains
        procedure :: sex => sex_str
        procedure :: fish_assign, fishivorous_assign, herbivorous_assign
        generic :: assignment(=) => fish_assign
        generic :: assignment(=) => fishivorous_assign
        generic :: assignment(=) => herbivorous_assign
    end type

    type, extends(fish) :: fishivorous
    contains
        procedure :: eat => eat_fish
    end type

    type, extends(fish) :: herbivorous
    contains
        procedure :: eat => eat_seaweed
    end type

contains
    function sex_str(this)
        implicit none
        class(fish), intent(in) :: this
        character(len=6) :: sex_str
        if (this%male) then
            sex_str = "male"
        else
            sex_str = "female"
        end if
    end function

    subroutine fish_assign(this, other)
        implicit none
        class(fish), intent(out) :: this
        type(fish), intent(in) :: other
        this%name = other%name
        this%male = other%male
    end subroutine

    subroutine fishivorous_assign(this, other)
        implicit none
        class(fish), intent(out) :: this
        type(fishivorous), intent(in) :: other
        this%name = other%name
        this%male = other%male
    end subroutine

    subroutine herbivorous_assign(this, other)
        implicit none
        class(fish), intent(out) :: this
        type(herbivorous), intent(in) :: other
        this%name = other%name
        this%male = other%male
    end subroutine

    subroutine eat_fish(this, other)
        implicit none
        class(fishivorous), intent(in) :: this
        class(fish) :: other
    end subroutine

    subroutine eat_seaweed(this, weed)
        implicit none
        class(herbivorous), intent(in) :: this
        class(seaweed) :: weed
    end subroutine
end module
