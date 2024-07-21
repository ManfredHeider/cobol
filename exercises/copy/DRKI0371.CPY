      * T13EX33   Stand: 19/08/98    16:19:23
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0371.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0371.CPv  $
      *
      *   Rev 2.1   Sep 05 2017 13:20:40   t13ac17
      *AKI01538: G20 - AML-Änderungen Standard OZP
      *
      *   Rev 2.0   Mar 23 1999 15:52:50   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0371                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0371                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.08.2017 / 10:39:37 / T13AC17
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01538!15.08.17!2.14! neu: LTVTLTYKZN-KNR       ! GWA      *
      *         !        !    !      LTVTLTYKZN-KTY       !          *
      *         !        !    !      LTBZBRFGRP-KNR       !          *
      *         !        !    !      LTBZBRFGRP-KTY       !          *
      *         !        !    !      LTABUKTKZN           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0371
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0371.
           05  PI-DRKI0371-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0371-LTAVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0371-LTAVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI0371.
           05  PO-DRKI0371-RC                     PIC  X(003).
               88 PO-DRKI0371-RC-OK               VALUE 'OK '.
               88 PO-DRKI0371-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

