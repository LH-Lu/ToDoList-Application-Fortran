! Functions of to-do application
! 1) Save user task to a file
! 2) Read user file on start up. Display "no tasks" if no tasks, else list and display tasks [DONE]
! 3) Add / Remove tasks. If no tasks, display "no tasks to delete". [DONE]
! Need to be able to catch error input by user when deleting (enter char instead of int, enter int less than or equal 0, enter int greater than num of tasks) [DONE]] or changing functions [DONE]
    
program main
    implicit none
    
    ! Declaration (for std fortran ops)
    integer :: endcase, ErrorNum
    
    ! Declaration (const)
    integer, parameter :: MaxTaskStringLength = 500
    integer, parameter :: MaxNumberTasks = 100
    character (len = *), parameter :: ToDoFileName = "ToDo.txt"
    
    ! Declaration (Functions)
    
    ! Declaration (others)
    character (len = MaxTaskStringLength), dimension (MaxNumberTasks) :: ToDoData
    integer :: NumTasks = 0
    integer :: UserProgOption = 0
    
    ! Step 1: Open file if it exists, else create a new file. Copy all data to memory. Close file.
    print *, "Hello and welcome!"
    call ReadWriteData (MaxTaskStringLength, MaxNumberTasks, ToDoFileName, ToDoData, NumTasks, 'R')
    
    do
        ! Step 2: Print list of tasks to console
        print *, "--- PROGRAM DIRECTORY ---"
        print *, "Here are your tasks:"
        call PrintTasks (MaxTaskStringLength, MaxNumberTasks, ToDoData, NumTasks)
        
        ! Step 3: Asks user for which program function to use
        print *, " "
        print *, "Welcome to the ToDo-er program. What would you like to do?"
        print *, "1 - Add task. 2 - Delete task. 0 - Quit program"
        read (*, '(I)', iostat = ErrorNum) UserProgOption
        print *, " "
        
        if (ErrorNum /= 0) then
            UserProgOption = -1
        end if
    
        select case (UserProgOption)
        case (0)
            print *, "Ending Program! Press any key to close window. Good Bye!"
            exit
            
        case (1)
            print *, "--- ADDING TASKS ---"
            call ModifyTasks(MaxTaskStringLength, MaxNumberTasks, ToDoData, NumTasks, 'A')
            print *, " "
            
        case (2)
            print *, "--- DELETE TASKS ---"
            call ModifyTasks(MaxTaskStringLength, MaxNumberTasks, ToDoData, NumTasks, 'D')
            print *, " "
            
        case default
            print *, "WRONG INPUT! Please enter 0 - 2 only."
            print *, " "
            
        end select
    end do
    
    ! Step 4: Save memory to file. Close file
    call ReadWriteData (MaxTaskStringLength, MaxNumberTasks, ToDoFileName, ToDoData, NumTasks, 'W')
    
    ! Step 5: To close console window when enter any key
    read (*, '(I)', iostat = ErrorNum) endcase
    
end program main
    
! This subroutine opens or creates a new file and read data from it or write data to it, depending on the function of the procedure chosen    
subroutine ReadWriteData(MaxTaskStringLength, MaxNumberTasks, ToDoFileName, ToDoData, NumTasks, FnTask)
    implicit none
    ! Declare arguments
    integer, intent (in) :: MaxTaskStringLength
    integer, intent (in) :: MaxNumberTasks
    character (len = *), intent (in) :: ToDoFileName
    
    character (len = MaxTaskStringLength), dimension (MaxNumberTasks), intent (inout) :: ToDoData
    integer, intent (inout) :: NumTasks
    character, intent (in) :: FnTask
    
    ! Declare local
    integer :: ToDoFileUnitNumber
    integer :: ReadStat
    integer :: idx
    
    ! Open file
    select case (FnTask)
    case ('R') ! Read from file
        open (newunit = ToDoFileUnitNumber, file = ToDoFileName, status = "UNKNOWN") ! If status = "REPLACE" --> file overwritten with a blank doc
    case ('W') ! Write to file
        open (newunit = ToDoFileUnitNumber, file = ToDoFileName, status = "REPLACE") 
    end select
    
    ! Read contents into local variable / Write to file
    do idx = 1, MaxNumberTasks
        select case (FnTask)
        case ('R') ! Read from file
            read(ToDoFileUnitNumber, '(A)', iostat = ReadStat) ToDoData(idx)
        
            if (ReadStat /= 0) then
                NumTasks = idx - 1
                exit
            else
                ! Continue with loop
            end if
            
        case ('W') ! Write to file
            if (NumTasks < 0) then
                print *, "Error. Number of tasks less than 0. Closing file without saving."
                exit
            end if
            
            if (idx > NumTasks) then
                print *, "File saved and closed successfully."
                exit
            end if
            
            write(ToDoFileUnitNumber, '(A)') trim(ToDoData(idx))
        end select
    end do    
    
    ! Close file
    close(ToDoFileUnitNumber)
    
end subroutine ReadWriteData
    
! This subroutine prints out all the tasks loaded into memory
subroutine PrintTasks (MaxTaskStringLength, MaxNumberTasks, ToDoData, NumTasks) 
    implicit none
    ! Declare arguments
    integer, intent (in) :: MaxTaskStringLength
    integer, intent (in) :: MaxNumberTasks
    
    character (len = MaxTaskStringLength), dimension (MaxNumberTasks), intent (inout) :: ToDoData
    integer, intent (inout) :: NumTasks
    
    ! Declare local
    integer :: idx
    
    if (NumTasks == 0) then
        print *, "No tasks detected! Let's add some tasks! :D"
        return
    end if
    
    do idx = 1, NumTasks
        print '(I3, ")", A)', idx, trim(TodoData(idx))
    end do
    
end subroutine PrintTasks
    
! This subroutine either adds new tasks to the memory (ToDoData array) or removes tasks moves remaining takss up the order
subroutine ModifyTasks (MaxTaskStringLength, MaxNumberTasks, ToDoData, NumTasks, FnTask) 
    implicit none
    ! Declare arguments
    integer, intent (in) :: MaxTaskStringLength
    integer, intent (in) :: MaxNumberTasks
    
    character (len = MaxTaskStringLength), dimension (MaxNumberTasks), intent (inout) :: ToDoData
    integer, intent (inout) :: NumTasks
    character, intent (in) :: FnTask
    
    ! Declare local
    integer :: idx
    integer :: ReadStat
    integer :: DeleteTaskNum
    
    select case (FnTask)
    case ('A') ! Add tasks
        if (NumTasks >= MaxNumberTasks) then
            print *, "You have reached the maximum number of tasks! :("
            print *, "Please complete and delete some tasks to add more!"
            return
        end if
        
        print '(A, I3, A)', "What task would you want to add? [MAX ", MaxTaskStringLength," CHARACTERS]"
        read (*, '(A)', iostat = ReadStat) ToDoData(NumTasks + 1)
            
        if (ReadStat == 0) then
            NumTasks = NumTasks + 1
        else
            print *, "Error encountered. Please try again."
        end if
        
    case ('D') ! Delete tasks
        if (NumTasks == 0) then
            print *, "There are no tasks for you to delete :O"
            return
        end if
        
        do
            print *, "Which task would you like to delete?"
            read (*, '(I)', iostat = ReadStat) DeleteTaskNum
            
            if (ReadStat /= 0 .or. DeleteTaskNum > NumTasks .or. DeleteTaskNum < 1) then
                print '(A, I3, A)', "WRONG INPUT! Please enter 1 - ", NumTasks, " only."
                print *, " "
            else
                exit
            end if
        end do
        
        do idx = DeleteTaskNum, NumTasks
            ToDoData(idx) = ToDoData(idx + 1)
        end do
        
        NumTasks = NumTasks - 1
        
    end select
    
end subroutine ModifyTasks
    