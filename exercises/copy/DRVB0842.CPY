      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0842.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0842.CPv  $
      *
      *   Rev 2.2   Mar 17 2008 09:49:02   t13ex13
      *FVB00669: EU-VR: Neue Schnittstelle AM
      *
      *   Rev 2.1   06 Jul 2007 09:33:22   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System: Registrierung
      *
      *   Rev 2.0   15 Feb 2007 14:42:46   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00906!15.02.07!2.0 ! NEU                       !GUTFLEISCH*
      * FVB00669!05.03.08!2.1 ! HRAbteilung hinzugefügt   !GUTFLEISCH*
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEIBESCHREIBUNG DER AN DIE      *
      *    AM (WEITER: DIHK) ZU ÜBERMITTELNDEN REGISTRIERUNGSDATEI   *
      *                                                              *
      ****************************************************************
       01 DRVB0842-SATZ.
                 05 DRVB0842-VVR-MELDESYS-EIN-SST.
                     07 DRVB0842-MELDESTATUS            PIC XXX.
                     07 DRVB0842-SENDERVU              PIC S9(8) BINARY.
                     07 DRVB0842-MELDUNGVU             PIC S9(8) BINARY.
                     07 DRVB0842-REGNR                  PIC X(30).
                     07 DRVB0842-VUVERMITTLERKEY        PIC X(20).
                     07 DRVB0842-REGISTRIERUNGSART      PIC X.
                     07 DRVB0842-PERSONENDATEN.
                        09 DRVB0842-ANREDE              PIC X(30).
                        09 DRVB0842-TITEL               PIC X(30).
                        09 DRVB0842-NACHTITEL           PIC X(30).
                        09 DRVB0842-VORNAME             PIC X(50).
                        09 DRVB0842-NACHNAME            PIC X(50).
                        09 DRVB0842-NAMTITEL            PIC X(30).
                        09 DRVB0842-NAMZUS              PIC X(30).
                        09 DRVB0842-PLZ                 PIC X(10).
                        09 DRVB0842-ORT                 PIC X(50).
                        09 DRVB0842-STR                 PIC X(50).
                        09 DRVB0842-HAUSNR              PIC X(10).
                        09 DRVB0842-GEBDAT              PIC X(10).
                        09 DRVB0842-FUNKTION           PIC S9(8) BINARY.

                        09 DRVB0842-TELEFON             PIC X(30).
                        09 DRVB0842-FAX                 PIC X(30).
                        09 DRVB0842-EMAIL               PIC X(255).
                        09 DRVB0842-URL                 PIC X(255).
                     07 DRVB0842-FIRMADATEN.
                        09 DRVB0842-NAME1               PIC X(50).
                        09 DRVB0842-NAME2               PIC X(50).
                        09 DRVB0842-NAME3               PIC X(50).
                        09 DRVB0842-NAME4               PIC X(50).
                        09 DRVB0842-NAME5               PIC X(50).
                        09 DRVB0842-NAME6               PIC X(50).
                        09 DRVB0842-PLZ                 PIC X(10).
                        09 DRVB0842-ORT                 PIC X(50).
                        09 DRVB0842-STR                 PIC X(50).
                        09 DRVB0842-HAUSNR              PIC X(10).
                        09 DRVB0842-RECHTSFORMNR       PIC S9(8) BINARY.

                        09 DRVB0842-TELEFON             PIC X(30).
                        09 DRVB0842-FAX                 PIC X(30).
                        09 DRVB0842-EMAIL               PIC X(255).
                        09 DRVB0842-URL                 PIC X(255).
                        09 DRVB0842-HRGERICHT          PIC S9(8) BINARY.
                        09 DRVB0842-HREINTRAG           PIC X(30).
050308                  09 DRVB0842-HRABTEILUNG         PIC X.
                     07 DRVB0842-ANZ-SPARTEN            PIC 99.
                     07 DRVB0842-SPARTEN.
                        09 DRVB0842-SPARTE              OCCURS 50 TIMES.
                           11 DRVB0842-SPARTEX          PIC X(4).
                     07 DRVB0842-HAFTUNGSUEBERNAHMEAB   PIC X(10).
                     07 DRVB0842-TAETIGKEITSART        PIC S9(8) BINARY.
                     07 DRVB0842-VUHAFTUNG             PIC S9(8) BINARY.
                     07 DRVB0842-LOESCHUNGAB            PIC X(10).
                     07 DRVB0842-CREATETIME             PIC X(26).
                     07 DRVB0842-EXTERNALMESSAGEID      PIC X(30).
                     07 DRVB0842-ANZ-TAETSTAATEN        PIC 99.
                     07 DRVB0842-TAETIGKEITSSTAATEN.
                        09 DRVB0842-TAETIGKEITSSTAAT    OCCURS 60 TIMES.
                           11 DRVB0842-LANDNRA         PIC S9(8) BINARY.
                     07 DRVB0842-ANZ-NIEDERLANSCHR PIC 99.
                     07 DRVB0842-NIEDERLANSCHR.
                        09 DRVB0842-NIEDERLANSCHRIFT
                                               OCCURS 9 TIMES.
                           11 DRVB0842-LANDNR          PIC S9(8) BINARY.
                           11 DRVB0842-FIRMA-NAME1      PIC X(50).
                           11 DRVB0842-FIRMA-NAME2      PIC X(50).
                           11 DRVB0842-FIRMA-NAME3      PIC X(50).
                           11 DRVB0842-FIRMA-NAME4      PIC X(50).
                           11 DRVB0842-FIRMA-PLZ        PIC X(10).
                           11 DRVB0842-FIRMA-ORT        PIC X(50).
                           11 DRVB0842-FIRMA-STR        PIC X(50).
                           11 DRVB0842-FIRMA-HAUSNR     PIC X(10).
                           11 DRVB0842-PERSON-TITEL     PIC X(30).
                           11 DRVB0842-PERSON-VORNAME   PIC X(50).
                           11 DRVB0842-PERSON-NACHNAME  PIC X(50).
                           11 DRVB0842-PERSON-PLZ       PIC X(10).
                           11 DRVB0842-PERSON-ORT       PIC X(50).
                           11 DRVB0842-PERSON-STR       PIC X(50).
                           11 DRVB0842-PERSON-HAUSNR    PIC X(10).
                 05 FILLER                              PIC X(17259).
      ******** ENDE COPY-MEMBER **************************************
