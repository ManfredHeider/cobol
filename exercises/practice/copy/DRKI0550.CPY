      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0550.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0550.CPv  $
      *
      *   Rev 2.1   Oct 01 1999 09:04:46   T13AE21
      *AKI00029
      *
      *   Rev 2.0   Feb 27 1999 12:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   07 Aug 1998 14:47:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0550                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                          *
      *    DATUM:    22.09.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00029!22.09.99! 01 ! NEUERSTELLUNG             ! PHILIPP  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0548                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0550.
          05 PI-DRKI0550-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0550-UFKT-CODE                PIC  9(002).
          05 PI-DRKI0550-DATEN.
            10 PI-DRKI0550-UNFVERTRAGSID          PIC X(013).
            10 PI-DRKI0550-UNFVERSTARLFDNR        PIC S9(4) COMP.
      *
       01 PO-DRKI0550.

          05 PO-DRKI0550-RC                       PIC  X(003).
             88 PO-DRKI0550-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0550-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI0550-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0550-DATEN.
            10 PO-DRKI0550-UNFBEITRAG         PIC S9(09)V9(2) COMP-3.
            10 PO-DRKI0550-UNFBEITRAGWKZ      PIC X(01).

      *
      ***** Ende Copy-Member DRKI0548 *****************************
      *---------------------------------------------------------------*
