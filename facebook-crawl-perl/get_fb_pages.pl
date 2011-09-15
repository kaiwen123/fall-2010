#!/usr/bin/perl
# download facebook info, wall and friend pages. 
# this process can be multi-threaded. 
use WWW::Mechanize; 
use HTTP::Cookies;
use CGI;
use get_friends qw( get_friends );

my $login = 'gsmsteve@gmail.com'; 
my $passwd = 'Gsm1011!'; 
my $fb_login_url = "https://login.facebook.com/login.php"; 
my $downloader = WWW::Mechanize->new(); 
$downloader->cookie_jar(HTTP::Cookies->new());

# login to facebook. 
$downloader->get($fb_login_url);
$downloader->submit_form(
    form_number => 1,
    fields => { email => $login, pass => $passwd },
    );
die unless ($downloader->success); 

# login debugging. 
# print "TEST" . $downloader->current_form()->attr($login) . "\n";
# print $downloader->success() . "\n"; 
# print $downloader->status() . "\n"; 
# print $downloader->response()->decoded_content . "\n";
# print $downloader->success;

# --------------------------------------------------
# Files to store the data. 
# --------------------------------------------------
open(INFODB, ">infodata.txt") || die("File open error."); 
open(WALLDB, ">walldata.txt") || die("File open error."); 
open(FNDDB, ">friendsdata.txt") || die("File open error."); 

# ----------------------------------------
# download info page. 
# ----------------------------------------
$infourl = "http://www.facebook.com/shumin.guo";
$downloader->get($infourl);
my $infocontent = $downloader->content();

$/ = "";			# paragraph mode. 

$infocontent =~ s/\n//g;
$infocontent =~ s/  +/ /g; 
$infocontent =~ s/<script[^>]*>.*?<\/script>//g;

print INFODB $infocontent . "\n";

# ----------------------------------------
# download wall page. 
# ----------------------------------------
$wallurl = $infourl . '?sk=wall';
$downloader->get($wallurl); 
my $wallcontent = $downloader->content(); 
$wallcontent =~ s/\n//g;
$wallcontent =~ s/  +/ /g; 
$wallcontent =~ s/<script[^>]*>.*?<\/script>//g;

print WALLDB $wallcontent . "\n";

# ----------------------------------------
# download friend lists. 
# ----------------------------------------
my $fndurl = $infourl . '?sk=friends';
$downloader->get($fndurl); 
my $fndlstcontent = $downloader->content(); 
$fndlstcontent =~ s/\n//g;
$fndlstcontent =~ s/  +/ /g; 
$fndlstcontent =~ s/<script[^>]*>.*?<\/script>//g;

print FNDDB $fndlstcontent . "\n";


close INFODB; 
close WALLDB; 
close FNDDB; 
