      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1895.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1895.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 15:14:20   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.1   Feb 27 2007 12:51:48   t13eX37
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.0   May 16 2006 08:30:14   t13eX37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1895                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1895                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         15.11.2011 / 15:27:27 / T13EX37
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
      *    FÜR DIE FUNKTION DRKI1895
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1895.
           05  PI-DRKI1895-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1895-KVBKIPERSONENID   PIC  X(0013).
             10 PI-DRKI1895-KVBVBPERSONENID   PIC  X(0013).
             10 PI-DRKI1895-KVBVBPERSONENROLLE   PIC  X(0001).
END   *

      *
       01  PO-DRKI1895.
           05  PO-DRKI1895-RC                     PIC  X(003).
               88 PO-DRKI1895-RC-OK               VALUE 'OK '.
               88 PO-DRKI1895-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1895-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1895-KVBKUNDEBETREUUNG   PIC  X(0001).
             10 PO-DRKI1895-KVBLOESCHKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
