      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0353.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0353.CPv  $
      *
      *   Rev 2.1   12 Sep 2001 14:19:34   t13ae46
      *APA00144: Neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:21:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0353                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0104                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    04.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.07.97!    ! NEU                       ! D.WILHELM*
      * APA00144!13.08.01! 2.1! NEUE FUNKTION DRPA0440    ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZXPA0104.
           05  PI-ZXPA0104-FUNKTION            PIC  X(008).
               88 PI-ZXPA0104-DRPA0354              VALUE 'DRPA0354'.
               88 PI-ZXPA0104-DRPA0355              VALUE 'DRPA0355'.
               88 PI-ZXPA0104-DRPA0371              VALUE 'DRPA0371'.
               88 PI-ZXPA0104-DRPA0379              VALUE 'DRPA0379'.
               88 PI-ZXPA0104-DRPA0494              VALUE 'DRPA0494'.
               88 PI-ZXPA0104-DRPA0495              VALUE 'DRPA0495'.
               88 PI-ZXPA0104-DRPA0630              VALUE 'DRPA0630'.
130801         88 PI-ZXPA0104-DRPA0440              VALUE 'DRPA0440'.
      *
      ******* ENDE COPY-MEMBER **************************************
