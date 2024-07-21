      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0717.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0717.CPv  $
      *
      *   Rev 2.3   Jun 22 2009 14:46:32   t13ex13
      *AVB01041: PGM - VB46 Pflege DG Allfinanz
      *
      *   Rev 2.2   19 Dec 2006 09:52:50   t13ae13
      *AVB00868: PGM - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.1   Nov 09 2006 08:58:22   t13ex43
      *AVB00868: PGM - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.0   Jul 02 2004 15:57:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0717                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0026                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    28.07.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00868!28.07.06!2.1 ! NEU                       ! KLEEMEIER *
      * AVB01041!11.05.09!2.2 ! NEUE FUNKTION FÜR ALLF    !GUTFLEISCH *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZJVB0026.
           05  PI-ZJVB0026-FUNKTION            PIC  X(008).
               88 PI-ZJVB0026-DRVB0719              VALUE 'DRVB0719'.
               88 PI-ZJVB0026-DRVB0939              VALUE 'DRVB0939'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *
