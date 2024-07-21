      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0571.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0571.CPv  $
      *
      *   Rev 2.1   Mar 01 2016 14:49:22   t13ex67
      *APA02208: Anpassungen Korrekturprogramm USt - MSS
      *
      *   Rev 2.0   May 27 1999 14:07:52   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 13:11:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0571                                      *
      *    --------------------------------------------------------   *
      *    PDPT-DATEN                                                 *
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
      * APA00012!01.08.98! 1.1! BUCHUNGSGRUND & KZ-EINHEIT! PARENT   *
      * APA00012!25.08.98! 1.2! ERGÄNZT UM                ! PARENT   *
      *         !        !    ! INSERT,KENNUNG,FIRST-KENN,!          *
      *         !        !    ! LAST-UPDATE               !          *
      * APA02208!01.02.16! 2.1! NEUES FELD PDPT-UST-JAHR  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PDPT.                   *
      *                                                              *
      ****************************************************************
      *
             30 PDPT-DATEN.
                35 PDPT-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PMPT-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PMPT-VERSION                     PIC S9(004) BINARY.
                35 PEST-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PDPT-BUCH-RFOLGE                 PIC S9(004) BINARY.
                35 PDPT-MA-ABR-DAT                  PIC  X(010).
                35 KNR-BUCHUNGS-STAND               PIC S9(004) BINARY.
                35 KNR-BUCHUNGSART                  PIC S9(004) BINARY.
                35 KNR-BUCHUNGS-GRUND               PIC S9(004) BINARY.
                35 KNR-KZ-EH-MAX                    PIC S9(004) BINARY.
                35 KNR-BERCH-WAEHRUNG               PIC S9(004) BINARY.
                35 PDPT-DGV                         PIC S9(004) BINARY.
                35 PDPT-PRODART                     PIC S9(004) BINARY.
                35 PDPT-FIRST-INSERT                PIC X(26).
                35 PDPT-FIRST-KENNUNG               PIC X(8).
                35 PDPT-LAST-UPDATE                 PIC X(26).
                35 PDPT-KENNUNG                     PIC X(8).
010216          35 PDPT-UST-JAHR                    PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

