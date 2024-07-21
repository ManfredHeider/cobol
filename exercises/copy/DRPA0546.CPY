      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0546.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA0546.CPv  $
      *
      *   Rev 2.8   Sep 21 2018 12:16:06   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.7   Mar 25 2014 13:02:20   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g):
      *
      *   Rev 2.6   Jan 10 2014 12:49:42   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.5   Mar 20 2006 08:37:42   t13ex44
      *APA00414: Erweiterung der Felder für Haftungszeitangaben bei
      * Central
      *
      *   Rev 2.4   09 May 2001 09:21:58   T13EX25
      *APA00132;PI-Bereich erweitert
      *
      *   Rev 2.3   May 18 1999 10:17:34   T13AE17
      *FAN00212 - 2. Übergabe der neuen Sourcen nach Rücknahme wg. ABEND
      *
      *   Rev 2.1   Apr 20 1999 14:37:24   T13AE17
      *AAN00219 - Einheiten nun mit 2 NK-Stellen
      *
      *   Rev 2.0   Apr 20 1999 11:00:26   T13AE17
      *Umstellung MFE - nun Ansi-Source
      *
      *   Rev 2.0   Apr 19 1999 12:52:36   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 10:32:28   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.0   01 Oct 1997 11:29:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0546                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<EINHEITEN BERECH. 2 >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                    *
      *    DATUM:    13.10.97                                         *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.10.97! 01 ! NEU                       ! JÄGER    *
      *         !        !    !                           !          *
      * APA00012!20.04.99! 2.1! EINHEITEN AUF 2-NACHKOMMA-! PARENT   *
      *         !        !    ! STELLEN UMGESTELLT        !          *
      *         !        !    !                           !          *
      * APA00132!07.05.01! 2.2! Neue Attribute einfügen   ! Jäger    *
      * APA00414!12.01.06! 2.3! HAFTZEITDAUER UND BELAS-  ! SCHEIDEL *
      *         !        !    ! TUNGSDAUER VIERSTELLIG    !          *
      * APA01526!24.10.13!2.6 ! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      * APA01526!04.03.14!2.7 ! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      * APA02548!26.07.18!2.8 ! NACHF. DIREKTION FAMILIE  ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0545                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
 
       01 PI-DRPA0546.
          05 PI-DRPA0546-PZVP-VERSION     PIC S9(004) BINARY.
          05 PI-DRPA0546-DATEN.
             10 PI-DRPA0546-GRUND-PROV    PIC S9(009)V99 PACKED-DECIMAL.
             10 PI-DRPA0546-GSMT-PROV     PIC S9(009)V99 PACKED-DECIMAL.
120106*      10 PI-DRPA0546-HAFTZEITDAUER    PIC X(03).
120106       10 PI-DRPA0546-HAFTZEITDAUER    PIC X(04).
120106*      10 PI-DRPA0546-BELASTUNGSDAUER  PIC X(03).
120106       10 PI-DRPA0546-BELASTUNGSDAUER  PIC X(04).
241013       10 PI-DRPA0546-BEGRSATZ-AP      PIC S9(009)V9(2) COMP-3.
241013       10 PI-DRPA0546-BEGRSATZ-PRPFLSU PIC S9(009)V9(2) COMP-3.
241013       10 PI-DRPA0546-EH-SATZ-ALLF     PIC S9(009)V9(2) COMP-3.
241013       10 PI-DRPA0546-PMPE-SAM-NR      PIC S9(09) COMP.
040314       10 PI-DRPA0546-KNR-VERM-GES     PIC S9(04) COMP.
             10 PI-DRPA0546-KOND-EH.
                COPY DRPA0548.
      *
       01 PO-DRPA0546.
          05 PO-DRPA0546-RC                         PIC  X(003).
             88 PO-DRPA0546-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0546-RC-REGEL-FALSCH              VALUE 'REF'.
             88 PO-DRPA0546-RC-K-WERT-FEHLT              VALUE 'KWF'.
          05 PO-DRPA0546-DATEN.
200499      10 PO-DRPA0546-EINH           PIC S9(006)V99 PACKED-DECIMAL.
200499      10 PO-DRPA0546-EINH-VWF       PIC S9(006)V99 PACKED-DECIMAL.
200499      10 PO-DRPA0546-EINH-VTF       PIC S9(006)V99 PACKED-DECIMAL.
200499      10 PO-DRPA0546-EINH-AVERM     PIC S9(006)V99 PACKED-DECIMAL.
200499      10 PO-DRPA0546-EINH-VM        PIC S9(006)V99 PACKED-DECIMAL.
200499      10 PO-DRPA0546-EINH-KD-BETR   PIC S9(006)V99 PACKED-DECIMAL.
200499      10 PO-DRPA0546-EINH-REST-STRK PIC S9(006)V99 PACKED-DECIMAL.
260718      10 PO-DRPA0546-EINH-NFOE      PIC S9(006)V99 PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
 

