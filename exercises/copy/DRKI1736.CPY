      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1736.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1736.CPv  $
      *
      *   Rev 2.2   Mar 16 2010 15:19:54   t13ex48
      *AKI00920: Teilbeträge für Hauptversicherung: Cobol-Sourcen
      *
      *   Rev 2.1   Nov 23 2004 16:50:54   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Nov 04 2004 15:38:40   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1736                                      *
      *    --------------------------------------------------------   *
      *    PI/PO Bereich für das List-OZP ZXKI0330                    *
      *    Lebenteilschicht                                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Pohl                                          *
      *    DATUM:    04.02.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00422!04.11.04! 01 ! NEUERSTELLUNG             ! P. Pohl  *
      *--------!---------!----!---------------------------!----------*
010310*AKI00920! 01.03.10! 02 ! Feldnamen LTSRATENBEITRAG ! HSE      *
      *        !         !    ! in LTSSCHICHTBEITR geaend.!          *
      *--------!---------!----!---------------------------!----------*
      * **************************************************************
      * LOGBUCH:                                                     *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME    *
      *--------------------------------------------------------------*
      * DBO-10383 !02.09.22! PO-DRKI1736-LTSSCHICHTZUGARTID! t13ex78 *
      *           !        ! mit aufgenommen               !         *
      *-----------+--------+-------------------------------+---------*
      *           !        !                               !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1736                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1736.
          05 PI-DRKI1736-PROGRAMM               PIC X(008).
          05 PI-DRKI1736-UFKT-CODE              PIC 9(002).
          05 PI-DRKI1736-DATEN.
             10 PI-DRKI1736-LTSVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI1736.

          05 PO-DRKI1736-RC                     PIC  X(003).
             88 PO-DRKI1736-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1736-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI1736-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1736-DATEN.
            10 PO-DRKI1736-LTSZAHLDYNERHOEH    PIC S9(4) COMP.
010310*     10 PO-DRKI1736-LTSRATENBEITRAG     PIC S9(11)V9(2) COMP-3.
010310      10 PO-DRKI1736-LTSSCHICHTBEITR     PIC S9(11)V9(2) COMP-3.
            10 PO-DRKI1736-LTSVERSSUMME        PIC S9(11)V9(2) COMP-3.
            10 PO-DRKI1736-LTSBEITRZUSTID      PIC X(4).
            10 PO-DRKI1736-LTSDATDYNERHOEH     PIC X(10).
            10 PO-DRKI1736-LTSSCHICHTZUGARTID  PIC X(1).
      *
      ***** Ende Copy-Member DRKI1736 *****************************

