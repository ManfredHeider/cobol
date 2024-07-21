      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1776.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1776.CPv  $
      *
      *   Rev 2.2   Oct 15 2007 18:12:46   t13ex48
      *AKI00677: Änderung der Zuordnung von unbetr. EDI-Dokumenten
      *
      *   Rev 2.1   Apr 01 2005 12:10:52   t13ex48
      *AKI00460: e-Post: Bereitstellg. der tägl. Änd. im Betreuungs
      *verhältnis
      *
      *   Rev 2.0   Mar 30 2005 09:05:30   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1776                                      *
      *    --------------------------------------------------------   *
      *    Funktions-Copy fuer UPKI0169                               *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00460!21.02.05!    ! Neu                       ! HSE      *
      * --------+--------+----+---------------------------+----------*
230807* AKI00677!23.08.07!    ! Neue Funktion DRKI2068    ! HSE      *
      * --------+--------+----+---------------------------+----------*
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
       01  PI-UPKI0169.
           05  PI-UPKI0169-FUNKTION            PIC  X(008).

             88   PI-UPKI0169-DRKI1777     VALUE 'DRKI1777'.
230807       88   PI-UPKI0169-DRKI2068     VALUE 'DRKI2068'.
      *
      ******* ENDE COPY-MEMBER **************************************

