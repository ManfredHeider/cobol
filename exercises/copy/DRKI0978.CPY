      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0978.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0978.CPv  $
      *
      *   Rev 2.2   Mar 16 2010 15:19:48   t13ex48
      *AKI00920: Teilbeträge für Hauptversicherung: Cobol-Sourcen
      *
      *   Rev 2.1   Feb 15 2000 14:51:24   T13AE21
      * 
      *
      *   Rev 2.0   24 Jan 2000 16:35:28   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0978                                      *
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
      * AKI00043!04.02.00! 01 ! NEUERSTELLUNG             ! PHILIPP  *
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
      *    FÜR DIE FUNKTION DRKI0978                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0978.
          05 PI-DRKI0978-PROGRAMM               PIC X(008).
          05 PI-DRKI0978-UFKT-CODE              PIC 9(002).
          05 PI-DRKI0978-DATEN.
             10 PI-DRKI0978-LTSVERTRAGSID       PIC X(013).
      *
       01 PO-DRKI0978.

          05 PO-DRKI0978-RC                     PIC  X(003).
             88 PO-DRKI0978-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0978-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI0978-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0978-DATEN.
            10 PO-DRKI0978-LTSZAHLDYNERHOEH    PIC S9(4) COMP.
010310*     10 PO-DRKI0978-LTSRATENBEITRAG     PIC S9(11)V9(2) COMP-3.
010310      10 PO-DRKI0978-LTSSCHICHTBEITR     PIC S9(11)V9(2) COMP-3.
            10 PO-DRKI0978-LTSVERSSUMME        PIC S9(11)V9(2) COMP-3.

      *
      ***** Ende Copy-Member DRKI0978 *****************************
      *---------------------------------------------------------------*
