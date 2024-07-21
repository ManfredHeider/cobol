      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2204.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2204.CPv  $
      *
      *   Rev 2.7   Apr 21 2015 14:02:14   t13ex27
      *AKI01352: VB-Nummer Prüfung 'neu' gestalten
      *
      *   Rev 2.6   03 Dec 2007 11:29:08   t13ex39
      *AKL00664: U P K I 0 2 0 9
      *
      *   Rev 2.0   02 Oct 2007 13:59:44   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1629                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FüR DAS UPRO UPKI0209                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00644!01.10.07! 1.0! NEU                       ! HORNUNG  *
      * AKI01352!20.02.15!2.7 ! DRKI_2206 entfernt        ! IFR      *
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
       01  PI-UPKI0209.
           05  PI-UPKI0209-FUNKTION            PIC  X(008).
 
             88   PI-UPKI0209-DRKI2205     VALUE 'DRKI2205'.
220215*      88   PI-UPKI0209-DRKI_2206    VALUE 'DRKI_2206'.
      *
      ******* ENDE COPY-MEMBER **************************************

