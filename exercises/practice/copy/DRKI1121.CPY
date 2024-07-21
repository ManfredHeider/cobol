      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1121.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1121.CPv  $
      *
      *   Rev 2.2   Aug 14 2012 14:59:04   t13ex14
      *AKI01154: automatische Vergabe neue KI-Produktnummer
      *
      *   Rev 2.1   31 Jan 2001 14:25:32   T13EX39
      *AKI00068 KBI
      *
      *   Rev 1.0   07 Jul 1998 11:49:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1121                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1121                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.07.2012 / 15:05:47 / T13EX14
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1121
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1121.
           05  PI-DRKI1121-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1121-PRODUKTID   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI1121.
           05  PO-DRKI1121-RC                     PIC  X(003).
               88 PO-DRKI1121-RC-OK               VALUE 'OK '.
               88 PO-DRKI1121-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1121-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1121-HAUPTPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI1121-PRODUKTSPARTEID   PIC S9(4) COMP.
             10 PO-DRKI1121-PRODUKTTEXT   PIC  X(0072).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
