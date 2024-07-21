      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0727.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0727.CPv  $
      *
      *   Rev 2.3   Nov 29 2010 15:36:20   t13ex13
      *ASL00367: SL-Dialog für Leistungszuschuss Allfinanz
      *
      *   Rev 2.2   Sep 22 2004 12:39:54   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.1   Sep 20 2004 15:18:08   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.0   Aug 16 2004 11:41:10   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0727                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0093                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    16.08.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00184!16.08.04! 2.1! NEU                       ! MUND     *
      * ASL00367!16.11.10! 2.2! LZU ALLFINANZ             !GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
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
       01  PI-ZXVB0093.
           05  PI-ZXVB0093-FUNKTION            PIC  X(008).
               88 PI-ZXVB0093-DRVB0728              VALUE 'DRVB0728'.
               88 PI-ZXVB0093-DRVB0740              VALUE 'DRVB0740'.
               88 PI-ZXVB0093-DRVB1002              VALUE 'DRVB1002'.
      *
      ******* ENDE COPY-MEMBER **************************************
