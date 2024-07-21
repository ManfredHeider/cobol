      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0455.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0455.CPv  $
      *
      *   Rev 2.1   12 Sep 2001 14:27:26   t13ae46
      *APA00144: Neue Funktion DRPA0741
      *
      *   Rev 2.0   Feb 27 1999 14:29:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:13:20   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:39:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0455                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0103                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.08.97!    ! NEU                       ! D.WILHELM*
      * Apa00122!22.07.98!1.02! NEUe funktion drpa1025    ! bohlinger*
      * APA00144!14.08.01! 2.1! NEUE FUNKTION DRPA0741    ! WANG     *
      *         !        !    !                           !          *
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
       01  PI-ZXPA0103.
           05  PI-ZXPA0103-FUNKTION            PIC  X(008).
               88 PI-ZXPA0103-DRPA0456              VALUE 'DRPA0456'.
               88 PI-ZXPA0103-DRPA1025              VALUE 'DRPA1025'.
140801         88 PI-ZXPA0103-DRPA0741              VALUE 'DRPA0741'.
      *
      ******* ENDE COPY-MEMBER **************************************
