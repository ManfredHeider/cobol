      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0269.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0269.CPv  $
      *
      *   Rev 2.4   Jan 13 2014 07:21:42   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.3   Mar 19 2003 09:21:08   t13ex21
      *APA00223 - Vergrößerung Prov-Multi
      *
      *   Rev 2.2   Nov 21 2002 17:37:18   t13ex22
      *APA00219: PROV-MULTI hinzugefügt
      *
      *   Rev 2.1   Apr 10 2002 13:32:04   t13ex43
      *APA00177 : PO-GSMT-PROV-DAT hinzu
      *
      *   Rev 2.0   Feb 27 1999 14:28:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:36:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:28:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0269                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GESAMTPROV. PRÜFEN  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    29.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!29.07.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!29.08.97!    ! ÄNDERUNG VON FELDNAMEN    ! WOLFF    *
      * AFPA0012!26.09.97!    ! ÄNDERUNG DER STELLEN BEI  ! WOLFF    *
      *         !        !    ! BETRÄGEN                  !          *
      * AFPA0012!18.05.98!    ! RC 'MFE' HINZU            ! PARENT   *
      * AFPA0012!        !    ! CRPA0018                  !          *
      *         !18.09.98!    !   - MELDUNGSFEHLER RAUS-  ! WOLFF    *
      *         !        !    !     GENOMMEN              !          *
      * APA00177!10.04.02!2.01! PO-GSMT-PROV-DAT HINZU    ! KLEEMEIER*
      * APA00219!13.11.02!2.02! PO-DRPA0269-PROV-MULTI    ! MUND     *
      * APA00223!12.03.03!2.03! PROV-MULTI VERGRÖSSERT    ! STORCH   *
      * APA01539!04.10.13!2.04! PI-BEREICH ERWEITERT      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0269                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0269.
          05 PI-DRPA0269-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0269-DATEN.
             10 PI-DRPA0269-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0269-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PI-DRPA0269-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PI-DRPA0269-PRODART                 PIC S9(004) BINARY.
             10 PI-DRPA0269-LAUFZEIT                PIC S9(004) BINARY.
             10 PI-DRPA0269-PMPP-PPFL-BEITRAG       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0269-PMPP-PPFL-SUMME         PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0269-PMPP-PG-GSMT-PROV       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
041013       10 PI-DRPA0269-PMPP-TARIFSCHL-GRP      PIC  X(015).
041013       10 PI-DRPA0269-PMPP-TARIF-KZ           PIC S9(004) BINARY.
      *
       01 PO-DRPA0269.
          05 PO-DRPA0269-RC                         PIC  X(003).
             88 PO-DRPA0269-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0269-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA0269-DATEN.
             10 PO-DRPA0269-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PO-DRPA0269-PROV-MULTI              PIC  9(003)V999999.
             10 PO-DRPA0269-KOND-GSP.
                COPY DRPA0396.
      *
      ******* ENDE COPY-MEMBER **************************************
