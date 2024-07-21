      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3252.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3252.CPv  $
      *
      *   Rev 2.1   Jun 07 2018 10:13:16   t13ae31
      *AKI01567: G10 Umstellung KSDS in DB2 OZP Kunde
      *
      *   Rev 2.0   Jan 30 2018 11:20:33 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3252                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3252                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.01.2018 / 11:56:44 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3252
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3252.
           05  PI-DRKI3252-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3252-G10KDROLLE   PIC S9(4) COMP.
             10 PI-DRKI3252-G10KDSPARTE   PIC  X(0003).
             10 PI-DRKI3252-G10KDVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3252.
           05  PO-DRKI3252-RC                     PIC  X(003).
               88 PO-DRKI3252-RC-OK               VALUE 'OK '.
               88 PO-DRKI3252-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

