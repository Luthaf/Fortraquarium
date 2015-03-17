module fishes
    implicit none
    private
    public :: fish

    type fish
        character(len=80) name
        logical male
    contains
        procedure :: sex => sex_str
        procedure :: assign
        generic :: assignment(=) => assign
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

    subroutine assign(this, other)
        implicit none
        class(fish), intent(out) :: this
        type(fish), intent(in) :: other
        this%name = other%name
        this%male = other%male
    end subroutine
end module
