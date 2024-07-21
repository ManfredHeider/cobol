      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0996.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0996.CPv  $
      *
      *   Rev 2.2   Mar 16 2010 15:19:42   t13ex48
      *AKI00920: Teilbeträge für Hauptversicherung: Cobol-Sourcen
      *
      *   Rev 2.1   31 Jan 2001 14:25:10   T13EX39
      *AKI00068 KBI
      *
      *   Rev 2.0   24 Jan 2000 16:35:28   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0996                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das List-OZP ZXKI0330                    *
      *    Lebenteilschicht                                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                          *
      *    DATUM:    04.02.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!16.02.00! 01 ! NEUERSTELLUNG             ! WHO      *
      *--------!---------!----!---------------------------!----------*
010310*AKI00920! 01.03.10! 02 ! Feldnamen LTSRATENBEITRAG ! HSE      *
      *        !         !    ! in LTSSCHICHTBEITR geaend.!          *
      *--------!---------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0996                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0996.
          05 PI-DRKI0996-PROGRAMM               PIC X(008).
          05 PI-DRKI0996-UFKT-CODE              PIC 9(002).
          05 PI-DRKI0996-DATEN.
             10 PI-DRKI0996-LTSVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI0996.

          05 PO-DRKI0996-RC                     PIC  X(003).
             88 PO-DRKI0996-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0996-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI0996-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0996-DATEN.
            10 PO-DRKI0996-LTSZAHLDYNERHOEH    PIC S9(4) COMP.
010310*     10 PO-DRKI0996-LTSRATENBEITRAG     PIC S9(11)V9(2) COMP-3.
010310      10 PO-DRKI0996-LTSSCHICHTBEITR     PIC S9(11)V9(2) COMP-3.
            10 PO-DRKI0996-LTSVERSSUMME        PIC S9(11)V9(2) COMP-3.
            10 PO-DRKI0996-LTSTARBEZEICHNUNG   PIC X(15).
            10 PO-DRKI0996-LTSDATDYNERHOEH     PIC X(10).

      *
      ***** Ende Copy-Member DRKI0996 *****************************
      *---------------------------------------------------------------*
