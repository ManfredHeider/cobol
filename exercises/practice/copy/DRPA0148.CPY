      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0148.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\DRPA0148.CPv  $
      *
      *   Rev 2.4   14 Nov 2000 14:38:48   T13AE34
      *AAN00285-PEW Hypo
      *
      *   Rev 2.3   May 18 1999 10:17:28   T13AE17
      *FAN00212 - 2. Übergabe der neuen Sourcen nach Rücknahme wg. ABEND
      *
      *   Rev 2.1   Apr 20 1999 14:37:20   T13AE17
      *AAN00219 - Einheiten nun mit 2 NK-Stellen
      *
      *   Rev 2.0   Apr 20 1999 11:00:22   T13AE17
      *Umstellung MFE - nun Ansi-Source
      *
      *   Rev 2.0   Apr 19 1999 12:52:34   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:48:12   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0148                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0148                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    28.10.1996                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!28.10.96! 01 ! NEU                       ! SCHÄFER  *
      *         !13.02.97!    ! ÄNDERUNG VGRUPPE, VSTATUS ! STORCH   *
      *         !        !    !                           !          *
      * APA00012!20.04.99!2.1 ! EINHEITEN AUF 2-NACHKOMMA-! PARENT   *
      *         !        !    ! STELLEN UMGESTELLT        !          *
      * AAN00285!13.11.00!2.4 ! PEW-HYPO                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0148
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0148 DES BUFFER-OZP ZYPA0002 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PA15, ANTRÄGE NACH FEHLERCODE.
      *
      ****************************************************************
      *
       01  PI-DRPA0148.
           05  PI-DRPA0148-UFKT                PIC  9(02).
           05  PI-DRPA0148-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0148-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0148-AUSWAHL-KRITERIEN.
               10 PI-DRPA0148-GESELL           PIC  9(02).
               10 PI-DRPA0148-VGRUPPE          PIC  9(03).
               10 PI-DRPA0148-S-VGRUPPE        PIC  X(01).
               10 PI-DRPA0148-VSTATUS          PIC  X(04).
               10 PI-DRPA0148-S-VSTATUS        PIC  X(01).
      *
       01  PO-DRPA0148.
           05  PO-DRPA0148-RC                  PIC  X(03).
               88 PO-DRPA0148-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0148-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0148-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0148-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA0148-SUM-PROV-BETRAG     PIC S9(09)V99.

           05  PO-DRPA0148-ERGEBNISZEILE.
131100*        10 PO-DRPA0148-PIN              PIC  9(07).
131100         10 PO-DRPA0148-PIN              PIC  9(15).
               10 PO-DRPA0148-KUNDEN-NAME      PIC  X(25).
               10 PO-DRPA0148-KUNDEN-VNAME     PIC  X(20).
               10 PO-DRPA0148-VSTATUS          PIC  X(04).
               10 PO-DRPA0148-WVORDATUM        PIC  X(10).
200499         10 PO-DRPA0148-EINHEITEN        PIC S9(06)V99.
               10 PO-DRPA0148-PROV-BETRAG      PIC S9(09)V99.
      *
      ******* ENDE COPY-MEMBER **************************************

