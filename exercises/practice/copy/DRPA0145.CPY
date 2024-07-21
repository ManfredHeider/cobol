      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0145.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0145.CPv  $
      *
      *   Rev 2.6   Nov 13 2006 14:57:34   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.5   21 Mar 2001 15:28:22   T13AE23
      *AAN00292 - PEW-Banken  VNR auf 15 Stellen 
      *                    vergrößert -
      *
      *   Rev 2.4   14 Nov 2000 14:37:58   T13AE34
      *AAN00285-PEW Hypo
      *
      *   Rev 2.3   May 18 1999 10:17:26   T13AE17
      *FAN00212 - 2. Übergabe der neuen Sourcen nach Rücknahme wg. ABEND
      *
      *   Rev 2.1   Apr 20 1999 14:37:18   T13AE17
      *AAN00219 - Einheiten nun mit 2 NK-Stellen
      *
      *   Rev 2.0   Apr 20 1999 11:09:26   T13AE17
      *Umstellung MFE - nun Ansi
      *
      *   Rev 1.1   Apr 09 1997 16:47:52   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0145                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0145                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BORK                                             *
      *    DATUM:    25.09.1996                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !25.09.96!    ! NEU                       ! BORK     *
      *         !12.02.97!    ! KZ,VSTAT,SUM-PROV EINGEFÜ.! STORCH   *
      *         !        !    !                           !          *
      * APA00012!20.04.99! 2.1! EINHEITEN AUF 2-NACHKOMMA-! PARENT   *
      *         !        !    ! STELLEN UMGESTELLT        !          *
      * AAN00285!13.11.00! 2.4! PEW-HYPO MIT VNR          ! HEIDER   *
      * AAN00292!21.03.01! 2.5! PEW-BANKEN VNR 15 STELLIG!! RAHNER   *
      * APA00452!13.11.06! 2.6! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0145
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0145.
           05  PI-DRPA0145-UFKT                PIC  X(02).
      *
           05  PI-DRPA0145-AUSWAHL-KRITERIEN.
               10 PI-DRPA0145-GESELL          PIC  9(002).
               10 PI-DRPA0145-AVERM           PIC  9(007).
      *
       01  PO-DRPA0145.
           05  PO-DRPA0145-RC                 PIC  X(003).
               88 PO-DRPA0145-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0145-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0145-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA0145-ERGEBNISZEILE.
131100*        10 PO-DRPA0145-PIN             PIC  9(007).
210301*        10 PO-DRPA0145-PIN             PIC  9(013).
210301         10 PO-DRPA0145-PIN             PIC  9(015).
               10 PO-DRPA0145-KUNDEN-NAME     PIC  X(025).
               10 PO-DRPA0145-KUNDEN-VNAME    PIC  X(020).
               10 PO-DRPA0145-VSTAT           PIC  X(004).
131106*        10 PO-DRPA0145-PROD-SCHL       PIC  9(002).
131106         10 PO-DRPA0145-PROD-PROD-SCHL  PIC S9(004) BINARY.
200499         10 PO-DRPA0145-EINHEITEN       PIC S9(006)V99.
               10 PO-DRPA0145-PROVISION       PIC S9(009)V99.
               10 PO-DRPA0145-KZ              PIC  X(001).
                  88 PO-DRPA0145-KZ-KORREKT                VALUE 'K'.
                  88 PO-DRPA0145-KZ-FEHLER                 VALUE 'F'.

           05  PO-DRPA0145-SUM-PROV-BETRAG     PIC S9(09)V99.
      *
      ******* ENDE COPY-MEMBER **************************************

