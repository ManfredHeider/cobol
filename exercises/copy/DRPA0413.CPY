      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0413.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0413.CPv  $
      *
      *   Rev 2.4   Feb 28 2017 09:54:34   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.3   Apr 07 2016 10:02:12   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.2   Jul 16 2015 15:00:36   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.1   Jan 10 2014 12:36:56   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.0   May 27 1999 13:30:32   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:33:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0413                                      *
      *    --------------------------------------------------------   *
      *    PMPT-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      * APA01526!09.04.13! 2.1! ERGÄNZUNGEN FÜR KALYPSO   ! BÖTTCHER *
      * APA01932!01.04.15! 2.2! NEUE ATTRIBUTE HINZU      ! STORCH   *
      * APA02220!10.03.16! 2.3! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPT-MA-GRD-PR-STZ       !          *
      * APA02275!31.01.17! 2.4! ERWEITERUNG FÜR FLEX-N    ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PMPT.                   *
      *                                                              *
      ****************************************************************
      *
             30 PMPT-DATEN.
                35 PMPT-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PMPT-VERSION                     PIC S9(004) BINARY.
                35 PVPT-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PVPT-VERSION                     PIC S9(004) BINARY.
                35 PMPR-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PMPR-VERSION                     PIC S9(004) BINARY.
                35 PMBE-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PRINTE                           PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PGEB-GES                         PIC S9(004) BINARY.
                35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
                35 KNR-PMPT-ART                     PIC S9(004) BINARY.
                35 PMPT-VSTAT                       PIC  X(004).
                35 PMPT-FAELLIG-VON                 PIC  X(010).
                35 PMPT-FAELLIG-BIS                 PIC  X(010).
                35 PMPT-PPFL-BEITRAG                PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PMPT-PPFL-SUMME                  PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PMPT-GSMT-PROV                   PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PMPT-GRUND-PROV                  PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PMPT-EINH                        PIC S9(006)V9(2)
                                                        PACKED-DECIMAL.
                35 PRKO-KOND-NR-GRP                 PIC S9(004) BINARY.
                35 PRKO-KOND-NR-EINH                PIC S9(004) BINARY.
                35 PRKO-KOND-NR-KDB                 PIC S9(004) BINARY.
 
100316*         35 PMPT-MA-GRD-PR-STZ               PIC S9(003)V9(3)
100316*                                                 PACKED-DECIMAL.
100316          35 PMPT-MA-GRD-PR-STZ               PIC S9(004)V9(4)
100316                                                  PACKED-DECIMAL.
010415          35 PMPT-PG-UST-BETRAG               PIC S9(011)V99
010415                                                  PACKED-DECIMAL.
010415          35 PMPT-PG-UST-SATZ                 PIC S9(003)V9(3)
010415                                                  PACKED-DECIMAL.
310117          35 PMPT-VSP-KOMBI-NR                PIC S9(005) BINARY.
310117          35 PMPT-VSP-KOMBI-BZ                PIC  X(003).
310117          35 PMPT-VSP-FN-BER                  PIC  X(001).
310117          35 PMPT-VSP-FN-REL                  PIC  X(001).
310117          35 PMPT-VSP-FN-FP-KZ                PIC  X(001).
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

