#!/usr/bin/perl

#Jonathan Xu
#Zain Shafique

use strict;
use warnings;
use Data::Dumper;
use Getopt::Std;

# opening file called 'Makefile' in current directory
if (!(open(FILE, "Makefile"))) {
    print "make: *** No makefile found.  Stop.\n";
    exit 1;
}

my %strsignal = (
    1 => "Hangup",
    2 => "Interrupt",
    3 => "Quit",
    4 => "Illegal instruction",
    5 => "Trace/breakpoint trap",
    6 => "Aborted",
    7 => "Bus error",
    8 => "Floating point exception",
    9 => "Killed",
   11 => "Segmentation fault",
   13 => "Broken pipe",
   14 => "Alarm clock",
   15 => "Terminated",
   16 => "Stack fault",
   17 => "Child exited",
   18 => "Continued",
   19 => "Stopped (signal)",
   20 => "Stopped",
   21 => "Stopped (tty input)",
   22 => "Stopped (tty output)",
   24 => "CPU time limit exceeded",
   25 => "File size limit exceeded",
   26 => "Virtual timer expired",
   27 => "Profiling timer expired",
   28 => "Window changed",
   29 => "I/O possible",
   30 => "Power failure",
   31 => "Bad system call",
);

my %opts;
getopts ("d", \%opts);
print("-- DEBUG OPTION ENABLED -- \n") if $opts{'d'};

# declare hash table to store macros
my %macros; 

# declare hash table to store targets and their respective dependencies
my %targets;

# declare hash table to store commands 
my %commands;

# keep track of last target matched
my $curr_target;

# array to hold dependencies for respective target
my @deps;

# array to hold command for respective target
my @command;

my $first_target = '';

my $modified = '';

# looping through each line of Makefile
while (my $line = <FILE>) {

    # regex to match for macros
    if ($line =~ /^(\S+)\s+=\s(.+)/) { 

        #assigning capture groups to variables $key and $value
        my ($key, $value) = ($1, $2);

        # print "$line";
        # print "1. $1 test\n";
        # print "2. $2 test\n\n";

        # inserting macro into hash table
        $macros{$key} = $value;
    }

    #regex to match for target/dependencies
    elsif ($line =~ /^(\S+)\s+:(.*)/) {

        #assigning capture groups to variables $t and $dependencies
        my ($t, $dependencies) = ($1, $2);

        #printing $t and $dependencies
        #print "key: $t, value: $dependencies\n";

        #initialize array called deps to contain matched capture groups
        my @deps = split(' ', $dependencies);

        #creating array reference to @deps to 
        #insert into target hash table
        my $dep_ref = \@deps;

        #inserting {target, array_ref} into target hash table
        $targets{$t} = $dep_ref;

        #setting global var $curr_target to $t
        $curr_target = $t;

        #print("curr_target in target: $curr_target\n\n");
    }

    #regex to match for commands
    elsif ($line =~ /^\t(.*)/) {
        
        
        #print "current_target in command: $curr_target\n";
        #print "command: $1\n\n";

        #initialize array called command to 
        #contain matched capture groups
        my @command = split(' ', $1);

        #create arry reference to @command to 
        #insert into command hash table
        my $command_ref = \@command;

        #print "command_ref: ";
        #print Dumper $command_ref;
        #print "\n";

        #insert {command, $command_ref}
        $commands{$curr_target} = $command_ref;
        
    }
    
}

#printing macro hash table
#print Dumper(\%macros);
#print "\n\n";
#print Dumper(\%targets);
#print "\n";
#print Dumper(\%commands);
#print "\n\n";

# reopen Makefile
open(FILE, "Makefile");

#print "ARGV: @ARGV\n";

my $num_args = @ARGV;

#print "num_args: $num_args\n";

#check if there are command args
if ($num_args != 0) {

    #check for debug mode
    if ($ARGV[0] eq '-d') {

        my $last_index = $num_args - 1;

        #loop through all command args after -d
        foreach my $item (@ARGV[1..$last_index]) {
            #print "foreach_1: $item\n";

            #call make on current target
            make($item);
        }
    }

    #command args only contain file names
    else {

        #loop through all commanda args
        foreach my $item (@ARGV) {
            #print "foreach_2: $item\n";

            #call make on current target
            make($item);
        }
    }

     #print "\n";
}

else {

    #call make for first target in Makefile
    while (my $line = <FILE>) {
    
        if ($line =~ /^(\S+)\s+:(.*)/) {
            
            $first_target = $1;
            #print "first_target: $first_target\n";

            make($1);

            last;
        }
    }   
}

sub make {

    #assign make argument to the variable $target 
    #argument: target name 
    my ($target) = @_;

    my $updated = "execute";

    #print "target_make: $target\n\n";

    #check if target exists in Makefile
    if (not (exists($targets{$target}))) {
        print "pmake: *** No rule to make target '$target'.  Stop.\n";
        exit 1;
    }

    #dependency array for current target 
    my @depen_array = @{$targets{$target}};

    #target has no command 
    if (not (exists($commands{$target}))) {
        #print "no commands\n";

        #parse the dependencies
        $updated = parse_depens($target, \@depen_array);

        #print "target: $target\n";
        #print "updated: $updated\n";

        if ($updated eq "!execute") {
            print "make: Nothing to be done for '$target'.\n";
        }
    }

    #target has command
    else {

        #command array for current target 
        my @command_array = @{$commands{$target}};
    
        #print "dependencies_array: @depen_array\n";

        #case 1: has dependencies
        if(@depen_array) {
            #iterate through dependencies of current target
            $updated = parse_depens($target, \@depen_array);
        }
        #print "updated: $updated\n";
        #run command for current target
        #print "updated_before: $updated\n";

        #print "updated_make: $updated\n\n";

        run_command($target, \@command_array) 
        unless $updated eq "!execute";
    }
}

sub parse_depens {

    my $target = shift;

    my $depen_array = shift;

    #print "target: $target\n";

    my @depen_array = @{$depen_array};

    #print "target_depens: $target\n\n";

    #print Dumper(\@depen_array);

    my $t_mod_time = time_stamp($target);
    

    #iterate through @dependencies for current target
    while (my ($index, $item) = each @depen_array) {
        #print "item: $item\n\n";
    
        #dependency is a target
        if(exists($targets{$item})) {
            
            #call make on target recursively
            make($item);
        }

        #dependency is a file
        else {
            #check if dependency has a macro and replaces it
            if ($item =~ /\${([^}]+)}/) {
                $item = replace_macro($1);

                #print "new_dependency: $item\n\n";
                
                my @new_dependency = split(' ', $item);
                #print "new_dependency_array: ";
                #print Dumper(\@new_dependency);

                return parse_depens($target, \@new_dependency);
            }

            #print "target: $target\n";

            #print "t_mod_time: $t_mod_time\n\n";

            #print "file name: $item\n";
            my $d_mod_time = time_stamp($item);
            #print "d_mod_time: $d_mod_time\n\n";

            

            #target is not a file or dependency 
            #mod time > target mod time
            if (not (defined ($t_mod_time)) || 
               ($d_mod_time > $t_mod_time)) {

                return "execute";
            }

            if ($index == scalar(@depen_array) - 1) {

                #print "target: $target\n";

                #print "first_target: $first_target\n";
                
                if (grep /$target/, @ARGV ) {
                    print "make: '$target' is up to date.\n";
                }
                elsif ($target eq $first_target) {
                    print "make: '$target' is up to date.\n";
                }   
                return "!execute";
            }  
        }
        #file was updated before the target
        #not last dependency
        next;  
    }
    #reached the end of dependency array
    #if reached here, 
    # - all files were not modified before target
    # - all dependencies which are targets were executed
    if ($modified eq 'true') {
        return "execute";
    }
    return "!execute";
}

sub run_command {

    my $target = shift;

    my $command_array = shift;

    my @command_array = @{$command_array};

    $modified = 'true';

    #print "array: @command_array\n\n";

    if (grep /\${[^}]+}/, @command_array) {

        my $new_command = '';
    
         while (my ($index, $item) = each @command_array) {

            #item is a macro
            if ($item =~ /\${([^}]+)}/) {

                #print "item1: $item\n";

                $new_command = $new_command.replace_macro($1);
            }

            else {
                #print "item2: $item\n";
                $new_command = $new_command."$item";
            }
            $new_command = $new_command." ";
            #print "new_command_progress: $new_command\n\n";
        }

        #print "new_command: $new_command";

        @command_array = split(' ', $new_command);

        #print Dumper(\@command_array);
    }

    my $last_index = scalar(@command_array) - 1;

    #check if command starts with '@'
    if ($command_array[0] eq '@') {
        system(@command_array[1..$last_index]);
    }
    
    else {
        #echo command to STDOUT
        #print "command does not start with @\n";
        if ($command_array[0] eq '-') {
            print "@command_array[1..$last_index]\n";
            system(@command_array[1..$last_index]);
        }
        else {
            print "@command_array\n";
            system(@command_array);
        }
    } 

    my $term_signal = $? & 0x7F;

    my $exit_status = ($? >> 8) & 0xFF;

    #print "exit_status = $exit_status\n";

    if ($exit_status == 1) {
        #print "exit_status = $exit_status\n";
        if ($command_array[0] eq '-') {
            print "make: [Makefile: $target] Error 1 (ignored)\n";
    
        }
        else {
            print "make: *** [Makefile: $target] Error 1\n";
        }
    }
}

sub replace_macro {

    my $replaced_macro = '';

    my ($target) = @_;

    my @macros = split(' ', $macros{$target});

    #print Dumper(\@macros);

    my $last_index = scalar(@macros) - 1;

    while (my ($index, $item) = each @macros) {
        if ($item =~ /\${([^}]+)}/) {
            #print "found: $1\n";
            $replaced_macro = $replaced_macro.replace_macro($1);
        }
        else {
            $replaced_macro = $replaced_macro."$item";
        }
        $replaced_macro = $replaced_macro." ";
    }
    return $replaced_macro;
}

sub time_stamp {
   my ($filename) = @_;
   my @stat = stat $filename;
   return @stat ? $stat[9] : undef;
}

# closing Makefile
close(FILE);