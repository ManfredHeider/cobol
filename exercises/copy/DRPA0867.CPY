      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0867.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0867.cpv  $
      *
      *   Rev 2.1   Mar 19 1999 18:05:10   T01JM2S
      *APA00012 - Fehlerbehebung FP 963
      *
      *   Rev 2.0   Feb 27 1999 14:29:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:39:24   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:47:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0867                                      *
      *    --------------------------------------------------------   *
      *    KONTROLLDATENSATZ FÜR KONTROLLSYSTEM                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    20.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!20.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!19.03.99!2.1 ! ERWEITERUNG AUF 80 BYTE   ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN DATENSATZ FÜR DAS KONTROLLSYSTEM  *
      *                                                              *
      ****************************************************************
      *
             30 KONTROLLDATENSATZ.
                35 KNR-FKT-GRUPPE                   PIC S9(004) BINARY.
                35 PDTA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 KNR-WAEHRUNG                     PIC S9(004) BINARY.
                35 ABSCHLUSS-DAT                    PIC  X(010).
190399          35 FILLER                           PIC  X(058).
      *
      ******* ENDE COPY-MEMBER **************************************
