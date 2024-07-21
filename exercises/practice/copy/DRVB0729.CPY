      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0729.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0729.CPv  $
      *
      *   Rev 2.3   Nov 29 2010 15:35:38   t13ex13
      *ASL00367: SL-Dialog für Leistungszuschuss Allfinanz
      *
      *   Rev 2.2   Sep 22 2004 12:40:02   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.1   Sep 20 2004 15:18:28   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.0   Aug 16 2004 15:14:32   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0729                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0093                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    11.08.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00184!11.08.04! 2.1! NEU                       ! T.MUND   *
      * ASL00367!15.11.10! 2.2! NEUE FUNKTION LZU ALLFI   !GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
      *         !        !    !                           !          *
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
       01  PI-ZWVB0093.
           05  PI-ZWVB0093-FUNKTION            PIC  X(008).
               88 PI-ZWVB0093-DRVB0730              VALUE 'DRVB0730'.
               88 PI-ZWVB0093-DRVB1001              VALUE 'DRVB1001'.
      *
      ******* ENDE COPY-MEMBER **************************************
