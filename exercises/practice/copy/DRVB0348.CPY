      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0348.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0348.CPv  $
      *
      *   Rev 2.1   Apr 26 2000 12:39:40   T13AE35
      *ASL00049 - Aend., neue Fkt. DRVB0395
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Jan 1999 12:19:52   T13AE42
      *ASL00036 - Neuerstellung
      *
      *   Rev 1.0   27 Oct 1998 16:56:58   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0348                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0026                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    17.12.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00017!17.12.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      * ASL00049!11.04.00! 02 ! neue Funktion DRVB0395    ! Walb     *
      *         !        !    !                           !          *
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
       01  PI-ZXVB0026.
           05  PI-ZXVB0026-FUNKTION            PIC  X(008).
               88 PI-ZXVB0026-DRVB0349              VALUE 'DRVB0349'.
110400         88 PI-ZXVB0026-DRVB0395              VALUE 'DRVB0395'.
      *
      ******* ENDE COPY-MEMBER DRVB0348 *****************************
