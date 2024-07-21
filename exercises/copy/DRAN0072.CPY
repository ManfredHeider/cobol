      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0072.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0072.CPv  $
      *
      *   Rev 2.1   16 Nov 2006 09:03:06   t13ae23
      *AAN00675: Erstellen Schnittstelle EGG zu AVS
      *
      *   Rev 2.0   Mar 18 2002 09:15:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00675!16.11.06! 2.1! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 AVS-SATZ-AUS.
           10 A-SST-TYP                        PIC X.
           10 A-REAL-DAT                       PIC X(19).
           10 A-VORSCHL-NR                     PIC X(17).
           10 A-KUNDE-ID                       PIC S9(18)   COMP-3.
           10 A-AVERM-NR                       PIC S9(9)    COMP.
           10 A-KEY.
              20 A-GES                         PIC S9(4)    COMP.
              20 A-VNR                         PIC X(15)    JUST.
           10 A-PIN                            PIC S9(9)    COMP.
           10 A-TARIF                          PIC X(10).
           10 A-PROD-PROD-SCHL1                PIC X(4)     JUST.
           10 A-PROD-PROD-SCHL2                PIC X(4)     JUST.
           10 A-PROD-PROD-SCHL3                PIC X(4)     JUST.
           10 A-KZ-MEHR-ALS3PROD               PIC X(1) .
           10 A-KUNDEN-NAME                    PIC X(25).
           10 A-KUNDEN-VORNAME                 PIC X(25).
           10 A-SUM-EINH                       PIC 9(7)V9   COMP-3.
           10 A-BEITRAG                        PIC 9(10)V99 COMP-3.
           10 A-SUMME                          PIC S9(9)    COMP.
           10 A-ANTRAG-DAT                     PIC X(10).
           10 A-ANTRAGERFASSUNG-DAT            PIC X(10).
           10 A-STATISTIK-MONAT                PIC 9(6).
           10 A-AKT-ANSTATUS                   PIC S9(4)    COMP.
           10 A-AKT-ANTRAG-STATUS-DAT          PIC X(10).
           10 A-FILLER                         PIC X(51).
      *
CCC009*    SKIP2
      ******************************************************************
      *    *** LAENGE 250 BYTES ***
      ******************************************************************

