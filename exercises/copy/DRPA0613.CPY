      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0613.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0613.CPv  $
      *
      *   Rev 2.2   Feb 07 2000 12:03:08   T13EX22
      *APA00077: Neue Funktion
      *
      *   Rev 2.1   Jun 02 1999 11:17:46   T13EX22
      *CRPA0033 UMGESETZT
      *
      *   Rev 2.0   May 21 1999 13:07:24   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 16:55:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0613                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0076                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                    *
      *    DATUM:    08.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!08.12.97!01.01! NEU                       ! JÄGER   *
      * APA00012!28.05.99!01.02! DRPA1247 NEU              ! MUND    *
      * APA00077!14.01.00! 2.0 ! NEUE FUNKTION DRPA1328    ! MUND    *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *
      *---------------------------------------------------------------*
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-UPPA0076.
           05  PI-UPPA0076-FUNKTION            PIC  X(008).
               88 PI-UPPA0076-DRPA0603              VALUE 'DRPA0603'.
               88 PI-UPPA0076-DRPA0675              VALUE 'DRPA0675'.
               88 PI-UPPA0076-DRPA0676              VALUE 'DRPA0676'.
               88 PI-UPPA0076-DRPA0677              VALUE 'DRPA0677'.
               88 PI-UPPA0076-DRPA0678              VALUE 'DRPA0678'.
               88 PI-UPPA0076-DRPA0679              VALUE 'DRPA0679'.
               88 PI-UPPA0076-DRPA0680              VALUE 'DRPA0680'.
               88 PI-UPPA0076-DRPA0681              VALUE 'DRPA0681'.
               88 PI-UPPA0076-DRPA0682              VALUE 'DRPA0682'.
               88 PI-UPPA0076-DRPA0683              VALUE 'DRPA0683'.
               88 PI-UPPA0076-DRPA0684              VALUE 'DRPA0684'.
               88 PI-UPPA0076-DRPA0685              VALUE 'DRPA0685'.
               88 PI-UPPA0076-DRPA0686              VALUE 'DRPA0686'.
               88 PI-UPPA0076-DRPA0688              VALUE 'DRPA0688'.
               88 PI-UPPA0076-DRPA0708              VALUE 'DRPA0708'.
280599         88 PI-UPPA0076-DRPA1247              VALUE 'DRPA1247'.
140100         88 PI-UPPA0076-DRPA1328              VALUE 'DRPA1328'.
      *
      ******* ENDE COPY-MEMBER **************************************
