      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0143.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0143.CPv  $
      *
      *   Rev 2.5   Nov 15 2006 08:59:16   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   14 Nov 2000 14:37:08   T13AE34
      *AAN00285-PEW Hypo
      *
      *   Rev 2.3   May 18 1999 10:17:22   T13AE17
      *FAN00212 - 2. Übergabe der neuen Sourcen nach Rücknahme wg. ABEND
      *
      *   Rev 2.1   Apr 20 1999 14:37:16   T13AE17
      *AAN00219 - Einheiten nun mit 2 NK-Stellen
      *
      *   Rev 2.0   Apr 20 1999 10:52:22   T13AE17
      *UMstellung MFE - nun Ansi
      *
      *   Rev 2.0   Apr 19 1999 12:52:30   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:47:28   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0143                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0143                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    26.09.1996                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !26.09.96!    ! NEU                       ! AUERBACH *
      *         !12.02.97!    ! KZ, SUM-PROV EINGEFÜGT    ! STORCH   *
      *         !        !    !                           !          *
      * APA00012!20.04.99! 2.1! EINHEITEN AUF 2-NACHKOMMA-! PARENT   *
      *         !        !    ! STELLEN UMGESTELLT        !          *
      * AAN00285!13.11.00! 2.4! PEW-HYPO MIT VNR          ! HEIDER   *
      * APA00452!01.11.06! 2.5! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0143
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0143.
           05  PI-DRPA0143-UFKT                PIC  9(02).
           05  PI-DRPA0143-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0143-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0143-AUSWAHL-KRITERIEN.
               10 PI-DRPA0143-GESELL           PIC  9(02).
               10 PI-DRPA0143-AVERM            PIC  9(07).
      *
       01  PO-DRPA0143.
           05  PO-DRPA0143-RC                  PIC  X(003).
               88 PO-DRPA0143-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0143-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0143-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0143-ANZAHL-ERG-ZEILEN   PIC  9(05).

      ***  AUSNAHME: DIESE ERGEBNSIZEILE ENSPRICHT NICHT DER DES
      ***            JOIN-OZP, DA IM BUFFER-OZP EINE VERARBEITUNG
      ***            STATTFINDET.
           05  PO-DRPA0143-ERGEBNISZEILE.
131100*        10 PO-DRPA0143-PIN              PIC  9(007).
131100         10 PO-DRPA0143-PIN              PIC  9(015).
               10 PO-DRPA0143-KUNDEN-NAME      PIC  X(025).
               10 PO-DRPA0143-KUNDEN-VNAME     PIC  X(020).
011106*        10 PO-DRPA0143-PROD-SCHL        PIC  X(010).
011106         10 PO-DRPA0143-PROD-PROD-SCHL-A13
011106                                         PIC  X(013).
200499         10 PO-DRPA0143-EINHEITEN        PIC S9(006)V99.
               10 PO-DRPA0143-PROVISION        PIC S9(009)V99.
               10 PO-DRPA0143-KZ               PIC  X(001).
                  88 PO-DRPA0143-KZ-KORREKT                VALUE 'K'.
                  88 PO-DRPA0143-KZ-FEHLER                 VALUE 'F'.
           05  PO-DRPA0143-SUM-PROV-BETRAG     PIC S9(09)V99.
      *
      ******* ENDE COPY-MEMBER **************************************

