      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0372.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0372.CPv  $
      *
      *   Rev 2.4   Jan 22 2019 14:50:52   t13ex78
      *AKI01641: Migration, Ermittlung, Aufber., Aktu.
      *
      *   Rev 2.3   Dec 19 2018 09:44:44   t13ex78
      *AKI01641: Migration, Ermittlung, Aufber., Aktu.
      *
      *   Rev 2.2   Jun 08 2018 12:30:16   t13ae31
      *AKI01567: G10 Umstellung auf KSDS in DB2 Pgm
      *
      *   Rev 2.1   Jun 07 2013 09:08:36   t13ex73
      *AKI01204: SEPA - DB-Online - Kfz
      *
      *   Rev 2.0   Mar 05 1999 11:36:36   T13EX41
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 17:03:00   P13SP13
      *Initial revision.
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KUNDEN-DATEN AMV VERÄNDERUNGSBESTAND        *
      *    ----------------------------------------------------        *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    SATZLÄNGE  319                                              *
150513*    SATZLÄNGE  472                                              *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *11.08.98!NEUERSTELLUNG                           !B.GOLÜCKE! 01 *
      *        !                                        !         !    *
140113*AKI01204!14.01.13! neue Felder für SEPA          !WMI      !    *
      *--------!----------------------------------------!--------_!----*
150513*AKI01204!15.02.13! Satzlänge Anpassung           !DDY      !    *
      *--------!----------------------------------------!--------_!----*
050218*AKI01567!05.02.18! nicht mehr benötigte Felder   !HHU      !    *
      *        !                     entfernt           !         !    *
      *--------!----------------------------------------!--------_!----*
271118*AKI01461!27.11.18!GeselLschaft eingebaut G10 G11 !HHU      !    *
      *        !                     entfernt           !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI0372.
 
           03  VERS-KUNDE.
 
               05 VERS-KUN-VERARB-KZ              PIC X(01).
      *           Verarbeitungskennzeichen
      *                I = Insert U = Update
 
               05  VERS-KUN-AEND-KZ               PIC 9(02).
               05  VERS-KUN-AEND-KZ-AENDKZ        PIC X(01).
      *            J = Änderung Änderungskennzeichen
 
               05  VERS-KUN-NACH-NAME             PIC X(40).
               05  VERS-KUN-NACH-NAME-AENDKZ      PIC X(01).
      *            J = Änderung Nachname
 
               05  VERS-KUN-VOR-NAME              PIC X(30).
               05  VERS-KUN-VOR-NAME-AENDKZ       PIC X(01).
      *            J = Änderung Vorname
 
               05  VERS-KUN-GEB-DATUM.
                   10  VERS-KUN-GEB-JH            PIC 9(02).
                   10  VERS-KUN-GEB-DAT.
                       15  VERS-KUN-GEB-JJ        PIC 9(02).
                       15  VERS-KUN-GEB-MM        PIC 9(02).
                       15  VERS-KUN-GEB-TT        PIC 9(02).
               05  VERS-KUN-GEB-DATUM-AENDKZ      PIC X(01).
      *            J = Änderung Geburtsdatum
 
               05  VERS-KUN-ANREDE-KZ             PIC 9(02).
               05  VERS-KUN-ANREDE-KZ-AENDKZ      PIC X(01).
      *            J = Änderung Anredekennzeichen
 
               05  VERS-KUN-GESCHLECHT            PIC 9(01).
               05  VERS-KUN-GESCHLECHT-AENDKZ     PIC X(01).
      *            J = Änderung Geschlecht
 
               05  VERS-KUN-STRASSE               PIC X(45).
               05  VERS-KUN-STRASSE-AENDKZ        PIC X(01).
      *            J = Änderung Strasse
 
               05  VERS-KUN-POSTFACH              PIC X(45).
               05  VERS-KUN-POSTFACH-AENDKZ       PIC X(01).
      *            J = Änderung Postfach
 
               05  VERS-KUN-POST-LZ               PIC X(09).
               05  VERS-KUN-POST-LZ-AENDKZ        PIC X(01).
      *            J = Änderung Postleitzahl
 
               05  VERS-KUN-WOHNORT               PIC X(40).
               05  VERS-KUN-WOHNORT-AENDKZ        PIC X(01).
      *            J = Änderung Wohnort
 
               05  VERS-KUN-AUSL-KZ               PIC X(03).
               05  VERS-KUN-AUSL-KZ-AENDKZ        PIC X(01).
      *            J = Änderung Auslandskennzeichen
 
050218**       05  VERS-KUN-TELEFON-NR            PIC X(15).
050218**       05  VERS-KUN-TELEFON-NR-AENDKZ     PIC X(01).
      *            J = Änderung Telefon
 
050218**       05  VERS-KUN-KOMM-KZ               PIC 9(02).
050218**       05  VERS-KUN-KOMM-KZ-AENDKZ        PIC X(01).
      *            J = Änderung Kommunikationsart
 
050218         05  FILLER                         PIC X(19).
               05  VERS-KUN-VERT-NR               PIC 9(15).
 
               05  VERS-KUN-ROLLE-KZ              PIC 9(02).
140113*            01=VN, 02=BZ, 03=VN+BZ
               05  VERS-KUN-ROLLE-KZ-AENDKZ       PIC X(01).
      *            J = Änderung Kundenrolle
 
140113         05  VERS-KUN-IBAN                  PIC X(034).
140113         05  VERS-KUN-IBAN-AENDKZ           PIC X(001).
140113
140113         05  VERS-KUN-BIC                   PIC X(011).
140113         05  VERS-KUN-BIC-AENDKZ            PIC X(001).
140113
050218**       05  VERS-KUN-NAME-AEND-KZ          PIC X(01).
050218**       05  VERS-KUN-NAME-AEND-KZ-AENDKZ   PIC X(01).
      *            J = Änderung Namensänderungskennzeichen
 
050218**       05  VERS-KUN-ADRESS-AEND-KZ        PIC X(01).
050218**       05  VERS-KUN-ADRESS-AEND-KZ-AENDKZ PIC X(01).
      *            J = Änderung Adressänderungskennzeichen
 
050218**       05  VERS-KUN-SONST-AEND-KZ         PIC X(01).
050218**       05  VERS-KUN-SONST-AEND-KZ-AENDKZ  PIC X(01).
      *            J = Änderung sonstiges Änderungskennzeichen
 
050218**       05  VERS-KUN-RES-FELD              PIC X(38).
050218**       05  VERS-KUN-RES-FELD-AENDKZ       PIC X(01).
      *            J = Änderung Rest ?
050218         05  FILLER                         PIC X(45).
140113****     05  FILLER                         PIC X(106).
271118         05  FILLER                         PIC X(104).
271118         05  VERS-KUN-GES                   PIC 99.
      ******************************************************************
      *          E n d e    Copy-Member DRKI0372                       *
      ******************************************************************
 
 

