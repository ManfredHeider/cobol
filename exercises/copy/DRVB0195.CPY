      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0195.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0195.CPv  $
      *
      *   Rev 2.2   Nov 28 2005 15:51:10   t13ex43
      *AVB00828: PGM - Adressänderungen für PC-Besitzer bereitstellen
      *
      *   Rev 2.1   Sep 28 1999 15:34:24   T13AE27
      *neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 15:58:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Apr 1998 16:29:02   T13EX30
      *AAS00005 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:14:00   P13SP99
      *Initial revision.
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0195                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0031                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    03.12.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!03.12.97! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      * AVB00458!01.09.99! 02 ! neue Funktion DRVB0443    ! Dürr     *
251105* AVB00828!25.11.05! 2.2! NEUE FUNKTION DRVB0795    ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0031.
           05  PI-ZWVB0031-FUNKTION            PIC  X(008).
               88 PI-ZWVB0031-DRVB0196              VALUE 'DRVB0196'.
               88 PI-ZWVB0031-DRVB0204              VALUE 'DRVB0204'.
               88 PI-ZWVB0031-DRVB0443              VALUE 'DRVB0443'.
251105         88 PI-ZWVB0031-DRVB0795              VALUE 'DRVB0795'.
      *
      ******* ENDE COPY-MEMBER DRVB0195 *****************************
