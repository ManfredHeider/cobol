      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0060.CPY  $
      *    $Revision:   1.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0060.CPv  $
      *
      *   Rev 1.2   Jul 03 2007 16:56:12   t13ex53
      *AKI00666: Einführung in MFE
      *
      *   Rev 1.1   19 May 1998 09:33:54   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:44:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
000100******************************************************************
000200*                                                                *
000300*    COPY-MEMBER ZU DEN AUFBEREITUNGSPROGRAMMEN KI-NEU           *
000400*     - Eingabedatei AMV-Vertragsdaten f?r kfz                    
000500*                                                                *
000600*    SATZLŽNGE   1630                                            *
000700*                                                                *
000800*    Anmerkung:                                                  *
000900*       L„nge des Vertrag-Allgemein 633                          *
001000*       L„nge des Kfz               607                          *
001100*                                                                *
001200*                                                                *
001300*                                                                *
001400*                                                                *
001500*----------------------------------------------------------------*
001600*    LOGBUCH                                                     *
001700*DATUM   !      ŽNDERUNG                         !NAME     !VERS.*
001800*AUFTRAG !                                       !         !     *
001900*--------!---------------------------------------!---------!-----*
002000*26.08.97!NEUERSTELLUNG                          !V.ZGUR-  ! 01  *
002100*KI-NEU  !                                       !VUJIC    !     *
002200*        !                                       !         !     *
002300*        !                                       !         !     *
002400******************************************************************
002500   03  amv-vertrag.                                               
002600                                                                  
002700     05  amv-vertr-allgemein.                                     
002800         10  amv-vall-aend             pic 9(02).                 
002900         10  amv-vall-vertrnr          pic 9(15).                 
003000         10  amv-vall-int-vertrnr      pic 9(07).                 
003100         10  amv-vall-pruef            pic 9(02).                 
003200         10  amv-vall-vbeginn          pic 9(08).                 
003300         10  amv-vall-vablauf          pic 9(08).                 
003400         10  amv-vall-zustand          pic x(01).                 
003500         10  amv-vall-blz              pic 9(11).                 
003600         10  amv-vall-kontonummer      pic 9(18).                 
003700         10  amv-vall-zahlungsw        pic x(01).                 
003800         10  amv-vall-bendbeitrag      pic S9(08)V99.             
003900         10  amv-vall-abschlverm       pic 9(07).                 
004000         10  amv-vall-aenddatum        pic 9(08).                 
004100         10  amv-vall-letzteaendd      pic 9(08).                 
004110         10  amv-vall-uhrzeit          pic 9(07).                 
004200         10  amv-vall-zahlungsart      pic x(01).                 
004300         10  amv-vall-stornodat        pic 9(08).                 
004400         10  amv-vall-tab occurs 10.                              
004504             20  amv-vall-zn-text      pic x(26).                 
004505             20  amv-vall-wert         pic x(01).                 
004506             20  amv-vall-dm-proc      pic s9(08)v99.             
004600         10  amv-vall-geb-vertr        pic x(01).                 
004601         10  AMV-S-A-VORT-LKZ          PIC X(03).                 
004602         10  AMV-S-A-VORT-PLZ          PIC X(05).                 
004603         10  AMV-S-A-VORT-STR          PIC X(21).                 
004604         10  AMV-S-A-VORT-ORT          PIC X(21).                 
004605         10  AMV-S-A-FILLER            PIC X(324).                
004800*    05  amv-vertr-kfz                 pic x(607).                
004810     05  amv-vertr-kfz.                                           
004900         10  amv-kfz-zschl            pic 9(02).                  
005000         10  amv-kfz-ztext            pic x(03).                  
005100         10  amv-kfz-amtkz            pic x(12).                  
005200         10  amv-kfz-zulassd          pic 9(08).                  
005300         10  amv-kfz-wagniskz         pic 9(03).                  
005400         10  amv-kfz-da-kh            pic 9(02).                  
005500         10  amv-kfz-beitragkh        pic 9(03).                  
005600         10  amv-kfz-da-kf            pic 9(02).                  
005700         10  amv-kfz-beitragkf        pic 9(03).                  
005800         10  amv-kfz-da-ku            pic 9(02).                  
005900         10  amv-kfz-herst-nr         pic x(04).                  
006000         10  amv-kfz-kz-kfz-typ       pic x(03).                  
006100         10  amv-kfz-fahrz-id         pic x(20).                  
006200         10  amv-kfz-kz-leist         pic x(01).                  
006300         10  amv-kfz-leistung         pic s9(05)v99.              
006400         10  amv-kfz-rkl-kh           pic x(04).                  
006500         10  amv-kfz-beitragkl-kh     pic x(06).                  
006600         10  amv-kfz-beitrag-zw-kh    pic s9(08)v99.              
006700         10  amv-kfz-typkl-kh         pic 9(02).                  
006800         10  amv-kfz-sb-vk            pic s9(08)v99.              
006900         10  amv-kfz-sb-tk            pic s9(08)v99.              
007000         10  amv-kfz-typkl-kf         pic 9(02).                  
007100         10  amv-kfz-rkl-kf           pic x(04).                  
007200         10  amv-kfz-bkl-vk.                                      
007210            15 amv-kfz-bkl-vk1        pic x(03).                  
007220            15 amv-kfz-bkl-vk2        pic x(03).                  
007300         10  amv-kfz-beitr-zw-kf      pic s9(08)v99.              
007400         10  amv-kfz-inv-summe-ku     pic s9(08)v99.              
007500         10  amv-kfz-tod-summe-ku     pic s9(08)v99.              
007600         10  amv-kfz-inv-prog-ku      pic 9(03).                  
007700         10  amv-kfz-beitr-zw-ku      pic s9(08)v99.              
007800         10  amv-kfz-tag-summe-ku     pic s9(08)v99.              
007900         10  amv-kfz-kfz-neuw         pic 9(07).                  
008000         10  amv-kfz-schutzbrief      pic x(10).                  
008100         10  amv-kfz-saison-kz        pic x(01).                  
008200         10  amv-kfz-typ-mod-text     pic x(50).                  
008300         10  amv-kfz-kfz-wkz-text     pic x(25).                  
008400         10  amv-kfz-kfz-saison-beg   pic 9(02).                  
008500         10  amv-kfz-kfz-saison-ende  pic 9(02).                  
008600         10  amv-kfz-vers-bed           occurs 3.                 
008700             20  amv-kfz-vbedingung       pic x(21).              
008800         10  amv-kfz-tarif-bez          occurs 3.                 
008900             20  amv-kfz-tarifb           pic x(07).              
009000         10  amv-kfz-tabelle            occurs 3.                 
009100             20  amv-kfz-filler           pic x(07).              
009200*        10  amv-kfz-filler            pic x(224).                
009300         10  amv-kfz-filler            pic x(379).                
