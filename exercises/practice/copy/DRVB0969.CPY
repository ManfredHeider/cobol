      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0969.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0969.CPv  $
      *
      *   Rev 2.2   Aug 26 2010 09:15:12   T13EX43
      *AAS00902: PGM - Rückbelastung Columbusbonus
      *
      *   Rev 2.1   Mar 17 2010 11:58:10   t13ex43
      *ASL00344: PGM - Auszahlung Columbus-Bonus
      *
      *   Rev 2.0   Mar 04 2010 16:03:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0969                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.03.2010                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00344!04.03.10!2.1 ! NEU                       ! KLEEMEIER *
100810* AAS00902!10.08.10!2.1 ! CBA-RÜCKBELASTUNG HINZU   ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0969 - ERMITTELN OFFENE ANSPRÜCHE AUS CBA     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0969.
           05  PI-DRVB0969-DATEN.
               10  PI-DRVB0969-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0969-KNR-GESELL-TYP    PIC S9(004) COMP.
               10  PI-DRVB0969-KNR-ANSPR-STATUS  PIC S9(004) COMP.
               10  PI-DRVB0969-ANSPR-VOM-JJJJ    PIC  9(004).
               10  PI-DRVB0969-ANSPR-VOM-MM      PIC  9(002).
100810         10  PI-DRVB0969-ANSPR-KZ          PIC  X(001).
      *
       01  PO-DRVB0969.
           05  PO-DRVB0969-RC                    PIC  X(003).
               88  PO-DRVB0969-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0969-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0969-DATEN.
               10  PO-DRVB0969-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0969-CBA-ANSPR-VOM     PIC  X(010).
               10  PO-DRVB0969-CBA-PART-VBNR     PIC S9(009) COMP.
               10  PO-DRVB0969-KNR-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0969-KTY-ANSPR-GRUND   PIC S9(004) COMP.
               10  PO-DRVB0969-CBA-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB0969-KTY-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0969-KNR-WAEHRUNG      PIC S9(004) COMP.
               10  PO-DRVB0969-CBA-PART-BETRAG   PIC S9(009)V99 COMP-3.
               10  PO-DRVB0969-CBA-PART-EH       PIC S9(009)V9  COMP-3.
               10  PO-DRVB0969-CBA-PART-SEIT     PIC  X(010).
               10  PO-DRVB0969-KTY-ANSPR-STATUS  PIC S9(004) COMP.
               10  PO-DRVB0969-KNR-ANSPR-STATUS  PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0969 ****************************************
      ****
