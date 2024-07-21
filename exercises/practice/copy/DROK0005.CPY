      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0005.CPv  $
      *
      *   Rev 2.1   19 Jan 2007 09:49:06   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Mar 17 2006 16:32:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: ZTOK0008                                   *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTOK0008                     *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.03.2006/12:53:31
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!10.03.06! 2.1! NEU                       !I. Frenzel*
081206* AKI00557!08.12.06! 2.1! DROK0061 HINZU (SELECT)   !KLEEMEIER *
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
       01  PI-ZTOK0008.
           05  PI-ZTOK0008-FUNKTION            PIC  X(008).

             88   PI-ZTOK0008-DROK0006     VALUE 'DROK0006'.
081206       88   PI-ZTOK0008-DROK0061     VALUE 'DROK0061'.
             
      *
      ******* ENDE COPY-MEMBER **************************************
      *
