      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0415.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0415.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:07:30   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.0   May 27 1999 13:32:24   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:33:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0415                                      *
      *    --------------------------------------------------------   *
      *    PDPE-DATEN                                                 *
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
      *         !        !    !                           !          *
      * Apa00012!01.08.98! 1.1! ST-NR hinzu ; POS-NR und  ! Parent   *
      *         !        !    ! PDPT-ID gelöscht(im Rahmen!          *
      *         !        !    ! der Denormalisierung)     !          *
      * APA01932!21.04.15! 2.1! UMSATZSTEUER ERGÄNZT      ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PDPE.                   *
      *                                                              *
      ****************************************************************
      *
             30 PDPE-DATEN.
010898*
010898*         35 PDPT-ID                          PIC S9(015)
010898*                                                 PACKED-DECIMAL.
010898*         35 PDPE-POSITIONS-NR                PIC S9(004) BINARY.
010898          35 PDPE-ST-NR                       PIC S9(009) BINARY.
                35 PDPE-NT-PBTRG-BRWG               PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PDPE-PROV-RCK-BRWG               PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PDPE-EINH                        PIC S9(006)V9(2)
                                                        PACKED-DECIMAL.
                35 PDPE-NT-PBTRG-AZWG               PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 PDPE-PROV-RCK-AZWG               PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
                35 KNR-AUSZ-WAEHRUNG                PIC S9(004) BINARY.
                35 PDPE-BUCH-RELEVANT               PIC  X(001).
210415          35 PDPE-UST-PBTRG-BRWG              PIC S9(009)V9(2)
210415                                                  PACKED-DECIMAL.
210415          35 PDPE-UST-RCK-BRWG                PIC S9(009)V9(2)
210415                                                  PACKED-DECIMAL.
210415          35 PDPE-UST-PBTRG-AZWG              PIC S9(009)V9(2)
210415                                                  PACKED-DECIMAL.
210415          35 PDPE-UST-RCK-AZWG                PIC S9(009)V9(2)
210415                                                  PACKED-DECIMAL.
210415          35 PDPE-KZ-UST-BER                  PIC  X(001).
      *
      ******* ENDE COPY-MEMBER **************************************

