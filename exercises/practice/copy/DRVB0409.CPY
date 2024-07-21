      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0409.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRVB0409.CPv  $
      *
      *   Rev 2.3   Jul 24 2002 10:23:38   t13ae35
      *FSL00099 - neue Funktion DRVB0592 KL zum Zeitpunkt
      *
      *   Rev 2.2   08 Jun 2000 15:36:54   T13AE27
      *neue Funktion DRVB0485
      *
      *   Rev 2.1   Jun 04 1999 12:47:54   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 27 1999 13:56:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0409                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0040  KL                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    31.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!31.05.99! 01 ! NEU                       ! SCHMIDT  *
      * -------------------------------------------------------------*
      * FSL00026!05.06.00! 02 ! Neue Funktion DRVB0485    ! Duerr P. *
      * -------------------------------------------------------------*
      * FSL00099!23.07.02! 03 ! Neue Funktion DRVB0592    ! J.Hering *
      *         !        !    ! KL zum Zeitpunkt          !          *
      * -------------------------------------------------------------*
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
       01  PI-ZXVB0040.
           05 PI-ZXVB0040-FUNKTION            PIC X(008).
              88 PI-ZXVB0040-DRVB0410             VALUE 'DRVB0410'.
              88 PI-ZXVB0040-DRVB0485             VALUE 'DRVB0485'.
              88 PI-ZXVB0040-DRVB0592             VALUE 'DRVB0592'.
