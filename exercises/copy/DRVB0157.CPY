      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0157.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0157.cpv  $
      *
      *   Rev 2.1   Jun 07 1999 14:52:06   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 14 1999 12:31:56   T13AE35
      *NT-Version
      *
      *   Rev 1.0   Feb 19 1997 09:59:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0157                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0102                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MADERNER                                         *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0370!12.08.97!01  ! NEUERSTELLUNG             ! MADERNER *
011297* AAS00005!01.12.97!02  ! NEUE FUNKTION DRVB0190    ! SCHMIDT  *
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
       01  PI-UPVB0102.
           05  PI-UPVB0102-FUNKTION            PIC  X(008).
               88 PI-UPVB0102-DRVB0158              VALUE 'DRVB0158'.
011297         88 PI-UPVB0102-DRVB0190              VALUE 'DRVB0190'.
      *
      ******* ENDE COPY-MEMBER DRVB0157 *****************************
