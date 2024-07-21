      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0558.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0558.CPv  $
      *
      *   Rev 2.5   Jan 11 2012 09:08:14   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.4   Feb 26 2009 09:18:20   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.3   Feb 23 2009 08:08:20   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.2   Feb 23 2009 07:46:54   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.1   Oct 13 1999 19:01:04   T13EX19
      *fki00196 - neu - fkt copy für zxki0196
      *
      *   Rev 2.0   Feb 27 1999 12:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   07 Aug 1998 14:47:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0558                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0362                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: vzg                                            *
      *    DATUM:    14.10.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00196!14.10.99! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      * AKI00642!21.01.09!    ! VRTVRTRGSRT-TXT verlängert!WMI       *
      *         !        !    !                           !          *
201211* AKI01079!20.12.11!    ! Änderungen aufgrund Struk-!W.Misgeld *
      *         !        !    ! turänderung der Tabelle   !          *
      *         !        !    ! VERTRAGSARTBEZ            !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0558                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0558.
          05 PI-DRKI0558-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0558-UFKT-CODE                PIC  9(002).
          05 PI-DRKI0558-DATEN.
            07 PI-DRKI0558-VRTVERTRAGSID             PIC X(013).
      *
       01 PO-DRKI0558.

          05 PO-DRKI0558-RC                       PIC  X(003).
             88 PO-DRKI0558-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0558-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI0558-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0558-DATEN.
             10 PO-DRKI0558-VRTLFDNR   PIC S9(4) COMP.
             10 PO-DRKI0558-VRTVERSTARLFDNR   PIC S9(4) COMP.
201211*      10 PO-DRKI0558-VRTVERTRAGSARTID   PIC S9(4) COMP.
             10 PO-DRKI0558-VRTVERTRAGWAGNISKZ   PIC X.
201211*      10 PO-DRKI0558-VRTVERTRAGSART.
201211*         15 PO-DRKI0558-VRTVERTRAGSART-LEN   PIC S9(4) COMP.
210109*         15 PO-DRKI0558-VRTVRTRGSRT-TXT   PIC X(20).
201211*         15 PO-DRKI0558-VRTVRTRGSRT-TXT   PIC X(50).
201211       10 PO-DRKI0558-VRTVERTART-KTY            PIC S9(4) COMP.
201211       10 PO-DRKI0558-VRTVERTART-KNR            PIC S9(4) COMP.
      *
      ***** Ende Copy-Member DRKI0558 *****************************
