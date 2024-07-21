      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0888.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0888.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:28:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:48:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: drpa0888  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: opritz                                          *
      *    DATUM:    09.03.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.03.98! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0888                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ABH.SEGMENTE DATEI                   *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0888.
          10 PI-DRPA0888-ABHSEG-UFKT           PIC  X.
              88 PI-DRPA0888-UFKT-READ-FIRST   VALUE '1'.
              88 PI-DRPA0888-UFKT-READ-NEXT    VALUE '2'.
          10 PI-DRPA0888-ABHSEG-KEY.
              20 PI-DRPA0888-P2GES             PIC 99.
              20 PI-DRPA0888-P2INTVSNR         PIC 9(15).
              20 PI-DRPA0888-P2FAELLIG         PIC 9(06).
              20 PI-DRPA0888-P2PRODSCHL        PIC 9(02).
              20 PI-DRPA0888-P2LFDNR           PIC 9(02).
      *
       01 PO-DRPA0888.
          10 PO-DRPA0888-RC                    PIC  X(003).
             88 PO-DRPA0888-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0888-RC-NOT-FOUND                VALUE 'NFD'.
          10 PO-DRPA0888-ABHSEG-DATEN.
           20 PO-DRPA0888-SA                   PIC XX.
           20 PO-DRPA0888-KEY.
              30  PO-DRPA0888-GES              PIC XX.
              30  PO-DRPA0888-INTVSNR          PIC 9(15).
              30  PO-DRPA0888-KFAELLIG         PIC 9(6).
              30  PO-DRPA0888-PRODSCHL         PIC 99.
              30  PO-DRPA0888-LFDNR            PIC 99.
           20 PO-DRPA0888-SATZ.
              30  PO-DRPA0888-EKRIT.
                  40  PO-DRPA0888-EPGM         PIC X(8).
                  40  PO-DRPA0888-EDATUM       PIC 9(7)       COMP-3.
                  40  PO-DRPA0888-EZEIT        PIC 9(7)        COMP-3.
                  40  PO-DRPA0888-PSNR         PIC 9(7)        COMP-3.
              30  PO-DRPA0888-FREIKZ           PIC X.
              30  PO-DRPA0888-VSNR             PIC 9(15)     COMP-3.
              30  PO-DRPA0888-FAELLIG          PIC 9(7)      COMP-3.
              30  PO-DRPA0888-BUCHDATPART      PIC 9(7)      COMP-3.
              30  PO-DRPA0888-BUCHDATAML       PIC 9(7)      COMP-3.
              30  PO-DRPA0888-TERMIN           PIC 9(7)      COMP-3.
              30  PO-DRPA0888-FILLER           PIC X(17).
              30  PO-DRPA0888-BAD       REDEFINES PO-DRPA0888-FILLER.
                  40  PO-DRPA0888-BAD-LAUFZEIT PIC 99.
                  40  PO-DRPA0888-BAD-REST     PIC X(15).
              30  PO-DRPA0888-BFG       REDEFINES PO-DRPA0888-FILLER.
                  40  PO-DRPA0888-BFG-KDINH.
                   45 PO-DRPA0888-BFG-KDAVERM  PIC 9(8).
                  40  PO-DRPA0888-BFG-KDDAT    PIC 9(6).
                  40  PO-DRPA0888-BFG-REST     PIC X(3).
              30  PO-DRPA0888-ROLAND    REDEFINES PO-DRPA0888-FILLER.
                  40  PO-DRPA0888-ROLFILL      PIC X(8).
                  40  PO-DRPA0888-IPBETR       PIC 9(7)V99   COMP-3.
                  40  PO-DRPA0888-IPPROVS      PIC 99V9      COMP-3.
                  40  PO-DRPA0888-LAUFZEIT     PIC XX.
              30  PO-DRPA0888-GES91     REDEFINES PO-DRPA0888-FILLER.
                  40  PO-DRPA0888-GES91-GRUNDPROV PIC 9(02)V999.
                  40  PO-DRPA0888-GES91-REST   PIC X(12).
              30  PO-DRPA0888-LFDNRE           PIC 99.
              30  PO-DRPA0888-PRODART          PIC 9.
              30  PO-DRPA0888-PROVPFLSU        PIC S9(7)V99    COMP-3.
              30  PO-DRPA0888-PRORATASU        PIC S9(7)V99    COMP-3.
              30  PO-DRPA0888-PROVSATZ         PIC S999V9      COMP-3.
              30  PO-DRPA0888-GESPROVBETR      PIC S9(7)V99    COMP-3.
              30  PO-DRPA0888-EDKZ             PIC X.
              30  PO-DRPA0888-DDRKZ            PIC X.
              30  PO-DRPA0888-AUSKZ            PIC X.
              30  PO-DRPA0888-FILLER2          PIC X(7).
              30  PO-DRPA0888-STRUKANZ         PIC 99.
              30  PO-DRPA0888-STRUKTABELLE.
                  35  PO-DRPA0888-STRUKTAB OCCURS 2 TO 20 DEPENDING
                          ON PO-DRPA0888-STRUKANZ.
                      40  PO-DRPA0888-VBNR     PIC 9(7)        COMP-3.
                      40  PO-DRPA0888-STRUKSTU PIC 999.
                      40  PO-DRPA0888-DIFFPROV PIC S999V9      COMP-3.
                      40  PO-DRPA0888-PROVBETR PIC S9(5)V99    COMP-3.
                      40  PO-DRPA0888-PROVRUECK PIC S9(5)V99   COMP-3.
                      40  PO-DRPA0888-PROVEINH PIC S9(6)V9     COMP-3.
