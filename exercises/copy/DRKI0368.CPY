      * T13EX33   Stand: 19/08/98    16:18:30
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0368.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0368.CPv  $
      *
      *   Rev 2.3   Sep 05 2017 13:19:36   t13ac17
      *AKI01538: G20 - AML-Änderungen Standard OZP
      *
      *   Rev 2.2   Feb 22 2010 16:44:08   t13ex27
      *AKI00918: Kennzeichnung BU-Leistungsfall; Mapping und OZPs
      *
      *   Rev 2.1   May 12 2003 08:59:52   t13ae31
      *FKI00733 Select für Verslebentarif ZTKI0346
      *
      *   Rev 2.0   Mar 23 1999 15:52:44   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0368                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0346                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.08.2017 / 10:39:43 / T13AC17
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01538!15.08.17!2.3 ! neu: LTVTLTYKZN-KNR       ! GWA      *
      *         !        !    !      LTVTLTYKZN-KTY       !          *
      *         !        !    !      LTBZBRFGRP-KNR       !          *
      *         !        !    !      LTBZBRFGRP-KTY       !          *
      *         !        !    !      LTABUKTKZN           !          *
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
       01  PI-ZTKI0346.
           05  PI-ZTKI0346-FUNKTION              PIC  X(008).
 
 
             88   PI-ZTKI0346-DRKI0369     VALUE 'DRKI0369'.
             88   PI-ZTKI0346-DRKI0370     VALUE 'DRKI0370'.
             88   PI-ZTKI0346-DRKI0371     VALUE 'DRKI0371'.
             88   PI-ZTKI0346-DRKI1547     VALUE 'DRKI1547'.
             88   PI-ZTKI0346-DRKI2373     VALUE 'DRKI2373'.
 
      *
      ******* ENDE COPY-MEMBER **************************************

