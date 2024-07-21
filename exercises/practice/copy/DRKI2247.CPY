      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2247.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2247.CPv  $
      *
      *   Rev 2.1   06 May 2008 09:34:34   t13ex27
      *FKI00911: Verträge löschen- PMKI0413 (löschen DB2)
      *
      *   Rev 2.0   May 02 2008 16:30:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2247  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00911!02.05.08!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2247.
          05 PI-DRKI2247-PROGRAMM               PIC X(008).
          05 PI-DRKI2247-UFKT                   PIC 9(002).
          05 PI-DRKI2247-DATEN.
            10 PI-DRKI2247-FSAVERTRAGSID        PIC X(13).

       01 PO-DRKI2247.
          05 PO-DRKI2247-DATEN.
            10 PO-DRKI2247-FSAPERSONENID        PIC X(13).
            10 PO-DRKI2247-FSAPARTNERGESID      PIC S9(4) COMP.
            10 PO-DRKI2247-FSAGEMFREIBETRAG     PIC S9(9)V9(2) COMP-3.
            10 PO-DRKI2247-FSAFREIBETRAGVERBR   PIC S9(9)V9(2) COMP-3.
            10 PO-DRKI2247-FSAWAEHRUNGSKZ       PIC X(1).
            10 PO-DRKI2247-FSAANZDEPFREISTBET   PIC S9(4) COMP.
            10 PO-DRKI2247-FSAZINSABSCHSTEUER   PIC S9(9)V9(2) COMP-3.
            10 PO-DRKI2247-FSAFREIBETRAGABL     PIC X(10).
            10 PO-DRKI2247-FSASTUECKZINS        PIC S9(9)V9(2) COMP-3.
            10 PO-DRKI2247-FSAGUELTIGAB         PIC X(10).
            10 PO-DRKI2247-LETZTEAENDERUNG      PIC X(26).

          05 PO-DRKI2247-RC                       PIC  X(003).
             88 PO-DRKI2247-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2247-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI2247-RC-LEER                      VALUE 'LER'.
