      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0876.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0876.CPv  $
      *
      *   Rev 2.4   Jun 22 2020 12:12:24   t13ex71
      *ARW00147: Offensive 2020 - Jetzt erst recht: Neukunden/aktiv
      *ierte Einvertragskunden - Neue Sonderprov. (VB-System)(+) - 
      *W0018, ZJVB0028, etc. (BPR)
      *
      *   Rev 2.3   May 28 2020 18:06:20   t13ex71
      *FSL00288: Kundenleistungs-Bonus Allfinanz I und II + HBO
      *
      *   Rev 2.2   Apr 20 2012 08:20:32   T13EX43
      *ASL00419: PGM - KLZ für Central-Wechsler
      *
      *   Rev 2.1   14 Jan 2008 13:00:42   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   30 Nov 2007 13:39:04   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0876                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.12.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!07.12.07!2.1 ! NEU                       ! KLEEMEIER *
090312* ASL00419!09.03.12!2.2 ! CENTRAL-KZ HINZU          ! KLEEMEIER *
290420* VLA-501 !29.04.20!2.3 ! Ab KLZA-EH-NNE hinzu      ! B.Pretsch *
      *         !        !    ! wg. Fehler KLZA zurück    !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0876 - ERMITTELN OFFENE ANSPRÜCHE AUS KLZA    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0876.
           05  PI-DRVB0876-DATEN.
               10  PI-DRVB0876-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0876-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0876-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0876-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0876-ANSPR-VOM-MM      PIC  9(002).
090312         10  PI-DRVB0876-CENTRAL-KZ        PIC  X(001).
      *
       01  PO-DRVB0876.
           05  PO-DRVB0876-RC                    PIC  X(003).
               88  PO-DRVB0876-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0876-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0876-DATEN.
               10  PO-DRVB0876-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0876-KLZA-ANSPR-VOM    PIC  X(010).
               10  PO-DRVB0876-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0876-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0876-KLZA-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0876-KLZA-BEITRAG      PIC S9(009)V99 COMP-3.
               10  PO-DRVB0876-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0876-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0876-KLZA-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB0876-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0876-KNR-ANSPR-STATUS  PIC S9(004) COMP.
090312         10  PO-DRVB0876-KLZA-CENTRAL-KZ   PIC  X(001).
290420*        10  PO-DRVB0876-KLZA-EH-NNE       PIC S9(009)V9 COMP-3.
      *        10  PO-DRVB0876-KLZA-JEQF         PIC S9(003)V99 COMP-3.
      *        10  PO-DRVB0876-KLZA-ERF-INDX     PIC  X(001).
      *        10  PO-DRVB0876-KLZA-AS-CLUB-KZ   PIC  X(001).
      *        10  PO-DRVB0876-KLZA-BES-345-KZ   PIC  X(001).
      *        10  PO-DRVB0876-KLZA-STRUKTUR     PIC S9(004) COMP.
      *        10  PO-DRVB0876-KLZA-PRAXIS       PIC S9(004) COMP.
      *        10  PO-DRVB0876-KLZA-AUSTRITT-KZ  PIC  X(001).
      *        10  PO-DRVB0876-KLZA-BES-229-KZ   PIC  X(001).
      *        10  PO-DRVB0876-KLZA-BV-FEHLT-KZ  PIC  X(001).
      *        10  PO-DRVB0876-KLZA-PFAEND-KZ    PIC  X(001).
      *        10  PO-DRVB0876-KLZA-SPERRE-KZ    PIC  X(001).
290420*        10  PO-DRVB0876-KLZA-SPERRE-TEXT  PIC  X(040).
      ****
      **** ENDE COPY DRVB0876 ****************************************
      ****


