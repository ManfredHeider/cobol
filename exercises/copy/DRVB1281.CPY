      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1281.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1281.CPv  $
      *
      *   Rev 2.1   Jun 22 2020 12:11:04   t13ex71
      *ARW00147: Offensive 2020 - Jetzt erst recht: Neukunden/aktiv
      *ierte Einvertragskunden - Neue Sonderprov. (VB-System)(+) - 
      *W0018, ZJVB0028, etc. (BPR)
      *
      *   Rev 2.0   Jun 05 2020 13:38:18   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1281                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    02.06.2020                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-712 !02.06.20!2.27! NEU                       ! B.Pretsch *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1281 - ERMITTELN OFFENE ANSPRÜCHE AUS OFAUS   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1281.
           05  PI-DRVB1281-DATEN.
               10  PI-DRVB1281-UFKT-CODE         PIC  9(002).
               10  PI-DRVB1281-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB1281-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB1281-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB1281-ANSPR-VOM-MM      PIC  9(002).
      *
       01  PO-DRVB1281.
           05  PO-DRVB1281-RC                    PIC  X(003).
               88  PO-DRVB1281-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1281-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB1281-DATEN.
               10  PO-DRVB1281-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB1281-OFAUS-ANSPR-VOM   PIC  X(010).
               10  PO-DRVB1281-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB1281-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB1281-OFAUS-LAST-UPDATE PIC  X(026).
               10  PO-DRVB1281-OFAUS-ANSPR-BETRAG PIC S9(009)V99 COMP-3.
               10  PO-DRVB1281-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB1281-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      *
      ****
      **** ENDE COPY DRVB1281 ****************************************
      ****

