      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0521.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRVB0521.CPv  $
      *
      *   Rev 2.6   Apr 06 2010 13:32:14   t13ex13
      *ASL00344: RGT: SL08
      *
      *   Rev 2.5   Mar 18 2010 15:22:32   t13ex43
      *ASL00344: PGM - Ermittlung Columbus-Bonus
      *
      *   Rev 2.4   Feb 10 2009 15:06:20   t13ae27
      *AAS00796: Eliteclub Unternehmensaufbau 2009
      *
      *   Rev 2.3   Mar 11 2004 11:31:42   t13ae27
      *Scharfschaltung
      *
      *   Rev 2.2   08 Mar 2001 11:41:24   T13AE27
      *neu
      *
      *   Rev 2.1   08 Mar 2001 10:05:54   T13AE27
      *neu
      *
      *   Rev 2.0   22 Feb 2001 16:59:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0521                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Lese-OZP ZWVB0078                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Duerr                                            *
      *    DATUM:    23.02.2001                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!23.02.01! 2.1! NEU                       ! P.Dürr   *
      * AAS00443!20.03.04!2.2 ! neue Funktion DRVB0697    ! P. Dürr  *
      * AAS00796!06.01.09!2.3 ! neue Funktion DRVB0926    ! P. Dürr  *
190210* ASL00344!19.02.10!2.5 ! DRVB0966 HINZU            ! KLEEMEIER*
220310* ASL00344!22.03.10!2.6 ! DRVB0977 HINZU            !GUTFLEISCH*
      *         !        !    !                           !          *
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
       01  PI-ZWVB0078.
           05  PI-ZWVB0078-FUNKTION            PIC  X(008).
               88 PI-ZWVB0078-DRVB0522              VALUE 'DRVB0522'.
               88 PI-ZWVB0078-DRVB0523              VALUE 'DRVB0523'.
               88 PI-ZWVB0078-DRVB0528              VALUE 'DRVB0528'.
200104         88 PI-ZWVB0078-DRVB0697              VALUE 'DRVB0697'.
060109         88 PI-ZWVB0078-DRVB0926              VALUE 'DRVB0926'.
190210         88 PI-ZWVB0078-DRVB0966              VALUE 'DRVB0966'.
220310         88 PI-ZWVB0078-DRVB0977              VALUE 'DRVB0977'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
