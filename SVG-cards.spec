Summary: A set of playing cards in SVG
Name: SVG-cards
Version: 1.1
Release: 1
Group: Games
License: LGPL
Source: http://david.bellot.free.fr/files/%{name}-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-rpmroot
Provides: SVG-cards
Packager: David Bellot <bellot@stat.berkeley.edu>
Vendor: David Bellot
Buildarch: noarch

%description
This is a set of playing cards made in pure SVG with all kings, queens, jacks, numbers, jokers and backs of cards. 
This set of SVG files is intended to be used in games, figures, illustrations and web sites. The kings, queens and 
jacks are based on the french representation, because I find them beautiful. The name on the side of the characters' cards
are fictious and traditionnal. Sometimes they refer to a real of legendary character, sometimes not really. You'll find
Lancelot, Cesar (Caesar), Alexandre (Alexander) and David and so on...

%prep
%setup

%build
echo "Ready to use"

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/share/SVG-cards
mkdir -p $RPM_BUILD_ROOT/usr/share/doc/SVG-cards-%{version}
cp svg/*.svg $RPM_BUILD_ROOT/usr/share/SVG-cards
cp AUTHORS COPYING Changelog NEWS README SVG-cards.spec $RPM_BUILD_ROOT/usr/share/doc/SVG-cards-%{version}

%clean
rm -rf $RPM_BUILD_ROOT

%files
/usr/share/SVG-cards
%doc /usr/share/doc/SVG-cards-%{version}

%changelog
* Sun Jun 20 2004 David Bellot <bellot@stat.berkeley.edu>
	- 01_of_*.svg are now 01_of_*_01.svg or _01_of_*_A.svg depending of their number 1 or A
	- charatecters : resizing the white cover for the top-left and bottom-right cut-off symbol.
* Tue Jun 15 2004 David Bellot <bellot@stat.berkeley.edu>
	- * : first release
