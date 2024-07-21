      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0498.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0498.CPv  $
      *
      *   Rev 2.2   Jan 13 2014 07:21:48   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.1   Nov 09 2009 10:18:48   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   FEB 27 1999 14:29:08   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   07 AUG 1998 11:31:40   T13AE34
      *AAN00167 - VORVERTRAGSNUMMER
      *
      *   REV 1.0   23 JUL 1997 12:44:12   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0498  FUNKTION ERMITTELN KONDITION       *
      *                           FÜR GRUNDPROVISION                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!19.09.97! 01  !NEU                       !BOHLINGER *
      * AFPA0020!24.10.97!01.02!RC-KOND-DEF-NOK           !          *
      *         !        !     !ALLE KONDITIONSSÄTZE ER-  !          *
      *         !        !     !HALTEN VORZEICHEN         !          *
      *         !        !     !VERW. NESTED-COPY-STRECKE !BOHLINGER *
      * APA01539!01.10.13!02.02!ERWEITERUNG PI-BEREICH    ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0498                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0498.
          05 PI-DRPA0498-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA0498-DATEN.
             10 PI-DRPA0498-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRPA0498-PROD-PROD-SCHL      PIC S9(004) BINARY.
             10 PI-DRPA0498-STICHTAG            PIC  X(010).
             10 PI-DRPA0498-PRODART             PIC S9(004) BINARY.
             10 PI-DRPA0498-LAUFZEIT            PIC S9(004) BINARY.
011013       10 PI-DRPA0498-TARIFSCHL-GRP       PIC S9(009) BINARY.
011013       10 PI-DRPA0498-TARIFSCHL           PIC S9(009) BINARY.
011013       10 PI-DRPA0498-KNR-TARIF-KZ        PIC S9(004) BINARY.
      *
       01 PO-DRPA0498.
          05 PO-DRPA0498-RC                     PIC  X(003).
             88 PO-DRPA0498-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0498-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRPA0498-RC-SEL-KRIT-NOK          VALUE 'SKN'.
             88 PO-DRPA0498-RC-KOND-DEF-NOK          VALUE 'KDN'.
         05 PO-DRPA0498-DATEN.
             COPY DRPA0547.
      ******** ENDE COPY-MEMBER **************************************
