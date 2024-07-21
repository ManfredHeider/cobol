      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0716.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRVB0716.CPv  $
      *
      *   Rev 2.14   Feb 19 2021 14:57:50   t13ex18
      *ASL00640: Anforderung Versorgungsplan 2019
      *
      *   Rev 2.13   Apr 07 2020 21:03:30   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.12   Mar 24 2020 15:25:20   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.11   Mar 23 2020 16:41:46   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.10   Mar 20 2020 17:35:06   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.9   Dec 15 2016 10:59:32   t13ex57
      *AVB01430: Erweiterung der PIN SST
      *
      *   Rev 2.8   Sep 03 2009 11:36:14   t13ex57
      *AVB01063: KTY/KNR in PIN SST (ZOB)
      *
      *   Rev 2.7   May 29 2008 09:30:20   t13ex57
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - Z
      *OB UBO
      *
      *   Rev 2.6   06 Dec 2007 13:01:04   t13ex57
      *AAS00723: Erweiterung PIN-SST um Feld Mitarbeitertyp
      *
      *   Rev 2.5   27 Sep 2007 09:23:20   t13ex57
      *AAS00697: Erweiterung der PIN-SST
      *
      *   Rev 2.4   27 Jun 2007 11:41:54   t13ex57
      *AAS00697: Erweiterung der PIN-SST
      *
      *   Rev 2.3   Nov 09 2005 15:19:12   t13ex13
      *AVB00820: PIN-SST Adressdaten auch für ausgeschiedene VB
      *
      *   Rev 2.2   Dec 08 2004 15:54:00   t13ex57
      *AAS00500: Akt. Handynummer und Adresse im VB-Portal
      *
      *   Rev 2.1   Sep 22 2004 10:02:58   t13ex57
      *AVB00757: neue/erweiterte SST für Online-Berechtigungssystem
      *
      *   Rev 2.0   Jun 29 2004 11:56:44   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: SCHNITTSTELLENDATEI FÜR VB PORTAL             *
      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *****************************************************************
      *    VERSION   01   02.07.2001                                  *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVB00757!29.06.04! 01 ! NEU                       ! U.BOLL    *
      * AAS00500!03.12.04! 02 ! ERWEITERUNG UM STEUERNR   ! U.BOLL    *
      * AVB00820!27.09.05!2.3 ! Erweiterung um Bankverb., !Gutfleisch *
      *         !        !    ! Satzlänge um 100 erhöht!  !           *
310507* AAS00697!31.05.07!2.10! Erweiterung der SST um die! U. Boll   *
      *         !        !    ! VB Registriernummer,      !           *
      *         !        !    ! 2. Handynummer            !           *
010807* AAS00697!01.08.07!2.11! Erweiterung der SST um die! U. Boll   *
      *         !        !    ! Handelsregistereinträge   !           *
151107* AAS00723!15.11.07!2.12! Erweiterung der SST um den! U. Boll   *
      *         !        !    ! Mitarbeitertyp            !           *
010909* AVB01063!01.09.09!2.21! Erweitrg um KTY/KNR für DG! U.Boll    *
071216* AVB01430!07.12.16!2.24! Erweiterung um den Umsatz-! U.Boll    *
      *         !        !    ! steuer Status             !           *
110320* AVB01615!11.03.20!2.25! Erweiterung um folgende   ! G.Bruder  *
      *         !        !    ! Attribute:                !           *
      *         !        !    !-Bankverbindung Details    !           *
      *         !        !    !-Registrierungs Details    !           *
      *         !        !    !-AS Club mbL und Elite Club!           *
      *         !        !    !-regionalisierte DIR u §34d!           *
      *         !        !    !-steuerliche Status 5 Jahre!           *
      *         !        !    ! neue Gesamtlänge 2000     !           *
070420* AVB01615!07.04.20!2.13! KTY/KNR-REGDB aufgenommen ! G.Bruder  *
      *         !        !    ! Version jetzt korrigiert. !           *
050221* VLA-1878!05.02.21!2.14! Erweiterung um der SS um  ! G.Bruder  *
      *         !        !    ! Stellen-Typ Details.      !           *
080323* VLA-6303!08.03.23!2.15! Erweiterung der SSt um    ! G.Zelenka *
      *         !        !    ! Datum Stufe   +           !           *
      *         !        !    ! BV-GESCHUETZT             !           *
      * ENDE-LOGBUCH                                                  *
      ****************************************************************
      *
      *    SCHNITTSTELLE ZWISCHEN EINEM DATENERMITTLUNGSPROGRAMM
      *    UND DEM VB-PORTAL
      *

       01  DRVB0716.
           05 DRVB0716-VBPORT-ST-NR          PIC  9(007).
           05 DRVB0716-VBPORT-PIN            PIC  X(010).
           05 DRVB0716-VBPORTSTUFE           PIC  9(003).
           05 DRVB0716-VBPORTPRAXIS          PIC  9(001).
           05 DRVB0716-VBPORT-ADM-CLUB       PIC  X(001).
           05 DRVB0716-VBPORT-AS-CLUB        PIC  X(001).
110320     05 DRVB0716-VBPORT-AS-CLUB-MBL    PIC  X(001).
110320     05 DRVB0716-VBPORT-ELITE-CLUB     PIC  X(001).
           05 DRVB0716-VBPORT-DIR            PIC  9(004).
           05 DRVB0716-VBPORT-KAEPT-CLUB     PIC  X(001).
           05 DRVB0716-VBPORT-NAME           PIC  X(040).
           05 DRVB0716-VBPORT-VNAME          PIC  X(020).
           05 DRVB0716-VBPORT-ANREDE         PIC  X(011).
           05 DRVB0716-VBPORT-AKAD-TITEL     PIC  X(010).
           05 DRVB0716-VBPORT-ADELS-TITEL    PIC  X(010).
           05 DRVB0716-VBPORT-GEB-DATUM      PIC  X(010).
           05 DRVB0716-VBPORT-LAND-KZ-P      PIC  X(004).
           05 DRVB0716-VBPORT-PLZ-P          PIC  X(005).
           05 DRVB0716-VBPORT-ORT-P          PIC  X(040).
           05 DRVB0716-VBPORT-STRASSE-P      PIC  X(040).
310507*    05 DRVB0716-VBPORT-TELEFON-P      PIC  X(014).
310507*    05 DRVB0716-VBPORT-FAX-P          PIC  X(014).
310507     05 DRVB0716-VBPORT-TELEFON-P      PIC  X(026).
310507     05 DRVB0716-VBPORT-FAX-P          PIC  X(026).
           05 DRVB0716-VBPORT-LAND-KZ-B      PIC  X(004).
           05 DRVB0716-VBPORT-PLZ-B          PIC  X(005).
           05 DRVB0716-VBPORT-ORT-B          PIC  X(040).
           05 DRVB0716-VBPORT-STRASSE-B      PIC  X(040).
310507*    05 DRVB0716-VBPORT-TELEFON-B      PIC  X(014).
310507*    05 DRVB0716-VBPORT-FAX-B          PIC  X(014).
310507     05 DRVB0716-VBPORT-TELEFON-B      PIC  X(026).
310507     05 DRVB0716-VBPORT-FAX-B          PIC  X(026).
           05 DRVB0716-VBPORT-KZ-REG-BEIRAT  PIC  X(001).
           05 DRVB0716-VBPORT-KZ-REG-VORSITZ PIC  X(001).
           05 DRVB0716-VBPORT-ED-ANZ         PIC  X(004).
           05 DRVB0716-VBPORT-ABRECH-ADR     PIC  X(001).
310507*    05 DRVB0716-VBPORT-HANDY-NR       PIC  X(014).
310507     05 DRVB0716-VBPORT-HANDY-NR       PIC  X(026).
           05 DRVB0716-VBPORT-GES-TYP        PIC  9(001).
           05 DRVB0716-VBPORT-REGION         PIC  X(004).
110320     05 DRVB0716-VBPORT-REGION2        PIC  X(004).
           05 DRVB0716-VBPORT-VB-DAT         PIC  X(010).
           05 DRVB0716-VBPORT-DL-DAT         PIC  X(010).
031204     05 DRVB0716-VBPORT-STEUERNR       PIC  X(015).
110320     05 DRVB0716-VBPORT-B-TYP-1        PIC  9(004).
110320     05 DRVB0716-VBPORT-IBAN-1         PIC  X(034).
210905     05 DRVB0716-VBPORT-BANKNAME-1     PIC  X(040).
210905     05 DRVB0716-VBPORT-KTO-INHABER-1  PIC  X(040).
110320     05 DRVB0716-VBPORT-B-TYP-2        PIC  9(004).
110320     05 DRVB0716-VBPORT-IBAN-2         PIC  X(034).
110320     05 DRVB0716-VBPORT-BANKNAME-2     PIC  X(040).
110320     05 DRVB0716-VBPORT-KTO-INHABER-2  PIC  X(040).
110320     05 DRVB0716-VBPORT-B-TYP-3        PIC  9(004).
110320     05 DRVB0716-VBPORT-IBAN-3         PIC  X(034).
110320     05 DRVB0716-VBPORT-BANKNAME-3     PIC  X(040).
110320     05 DRVB0716-VBPORT-KTO-INHABER-3  PIC  X(040).
210905     05 DRVB0716-VBPORT-MAT-DAT-BIS    PIC  X(010).
310507     05 DRVB0716-VBPORT-REG-NR         PIC  X(030).
310507     05 DRVB0716-VBPORT-HANDY-NR2      PIC  X(026).
010807     05 DRVB0716-VBPORT-HRD-ORT        PIC  X(40).
010807     05 DRVB0716-VBPORT-HRD-EINTR      PIC  X(8).
010807     05 DRVB0716-VBPORT-HROE-EINTR-VA  PIC  X(25).
010807     05 DRVB0716-VBPORT-HROE-EINTR-VB  PIC  X(25).
151107     05 DRVB0716-VBPORT-KNR-MA-TYP     PIC  9(001).
151107     05 DRVB0716-VBPORT-BESCHR-POA     PIC  X(001).
070508     05 DRVB0716-VBPORT-NOTFALL-TYP    PIC  9(001).
070508     05 DRVB0716-VBPORT-MA-34C-AMT     PIC  X(40).
070508     05 DRVB0716-VBPORT-MA-34C-STRASSE PIC  X(40).
070508     05 DRVB0716-VBPORT-MA-34C-PLZ     PIC  X(09).
070508     05 DRVB0716-VBPORT-MA-34C-ORT     PIC  X(40).
310507     05 DRVB0716-VBPORT-REG-34D-NR     PIC  X(030).
310507     05 DRVB0716-VBPORT-REG-34D-DATE   PIC  X(010).
070508     05 DRVB0716-VBPORT-MA-34D-AMT     PIC  X(40).
070508     05 DRVB0716-VBPORT-MA-34D-STRASSE PIC  X(40).
070508     05 DRVB0716-VBPORT-MA-34D-PLZ     PIC  X(09).
070508     05 DRVB0716-VBPORT-MA-34D-ORT     PIC  X(40).
310507     05 DRVB0716-VBPORT-34D-EIG-REG-KZ PIC  X(02).
010909     05 DRVB0716-VBPORT-KTY-REGION     PIC  9(004).
010909     05 DRVB0716-VBPORT-KNR-REGION     PIC  9(004).
070420     05 DRVB0716-VBPORT-KTY-REGDB      PIC  9(004).
070420     05 DRVB0716-VBPORT-KNR-REGDB      PIC  9(004).
110320     05 DRVB0716-VBPORT-STEUER-JAHR-1  PIC  9(004).
110320     05 DRVB0716-VBPORT-STEUER-STAT-1  PIC  X(02).
110320     05 DRVB0716-VBPORT-STEUER-JAHR-2  PIC  9(004).
110320     05 DRVB0716-VBPORT-STEUER-STAT-2  PIC  X(02).
110320     05 DRVB0716-VBPORT-STEUER-JAHR-3  PIC  9(004).
110320     05 DRVB0716-VBPORT-STEUER-STAT-3  PIC  X(02).
110320     05 DRVB0716-VBPORT-STEUER-JAHR-4  PIC  9(004).
110320     05 DRVB0716-VBPORT-STEUER-STAT-4  PIC  X(02).
110320     05 DRVB0716-VBPORT-STEUER-JAHR-5  PIC  9(004).
110320     05 DRVB0716-VBPORT-STEUER-STAT-5  PIC  X(02).
050221     05 DRVB0716-VBPORT-STELLEN-TYP    PIC  X(04).
           05 DRVB0716-VBPORT-BST-TYP-P      PIC  9(001).
           05 DRVB0716-VBPORT-BST-TYP-B      PIC  9(001).
           05 DRVB0716-VBPORT-LAND-KZ-3      PIC  X(04).
           05 DRVB0716-VBPORT-PLZ-3          PIC  X(05).
           05 DRVB0716-VBPORT-ORT-3          PIC  X(40).
           05 DRVB0716-VBPORT-STRASSE-3      PIC  X(40).
           05 DRVB0716-VBPORT-TELEFON-3      PIC  X(26).
           05 DRVB0716-VBPORT-FAX-3          PIC  X(26).
           05 DRVB0716-VBPORT-BST-TYP-3      PIC  9(001).
           05 DRVB0716-VBPORT-LAND-KZ-4      PIC  X(04).
           05 DRVB0716-VBPORT-PLZ-4          PIC  X(05).
           05 DRVB0716-VBPORT-ORT-4          PIC  X(40).
           05 DRVB0716-VBPORT-STRASSE-4      PIC  X(40).
           05 DRVB0716-VBPORT-TELEFON-4      PIC  X(26).
           05 DRVB0716-VBPORT-FAX-4          PIC  X(26).
           05 DRVB0716-VBPORT-BST-TYP-4      PIC  9(001).
           05 DRVB0716-VBPORT-ZUSTELLUNG-P   PIC  9(002).
           05 DRVB0716-VBPORT-GEWERBE-P      PIC  9(001).
           05 DRVB0716-VBPORT-ZUSTELLUNG-B   PIC  9(002).
           05 DRVB0716-VBPORT-GEWERBE-B      PIC  9(001).
           05 DRVB0716-VBPORT-ZUSTELLUNG-3   PIC  9(002).
           05 DRVB0716-VBPORT-GEWERBE-3      PIC  9(001). 
           05 DRVB0716-VBPORT-ZUSTELLUNG-4   PIC  9(002).
           05 DRVB0716-VBPORT-GEWERBE-4      PIC  9(001).    
080323     05 DRVB0716-VBPORT-STUFE-DAT      PIC  X(10).
080323     05 DRVB0716-VBPORT-BV-GESCHUETZT  PIC  X(001).
080323     05 DRVB0716-FILLER                PIC  X(330).
      *
      ******** ENDE COPY-MEMBER (DRVB0716) ************
      *