      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1966.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1966.CPv  $
      *
      *   Rev 2.3   Nov 12 2015 13:16:40   t13ex27
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect.-Abgl
      *eich
      *
      *   Rev 2.2   Nov 22 2012 14:58:18   t13ex27
      *AOK00051: Projekt Aurora - Bestandsfunktionen-Abgleich
      *
      *   Rev 2.1   19 Jan 2007 09:48:24   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Oct 09 2006 13:52:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1966                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0199                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.12.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00556!08.12.06!2.1 ! NEU                       ! KLEEMEIER *
      * AOK00051!24.09.12!2.2 ! Steuersatz(Cloud) für     ! I.Frenzel *
      *         !        !    ! VN-Wechsel                !           *
      *         !        !    !                           !           *
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
       01  PI-UPKI0199.
           05  PI-UPKI0199-FUNKTION            PIC  X(008).
               88 PI-UPKI0199-DRKI1967              VALUE 'DRKI1967'.
240912         88 PI-UPKI0199-DRKI2696              VALUE 'DRKI2696'.
240912         88 PI-UPKI0199-DRKI2697              VALUE 'DRKI2697'.
240912         88 PI-UPKI0199-DRKI2698              VALUE 'DRKI2698'.
240912         88 PI-UPKI0199-DRKI2699              VALUE 'DRKI2699'.
271015         88 PI-UPKI0199-DRKI2700              VALUE 'DRKI2700'.
      *
      ******* ENDE COPY-MEMBER ****************************************

