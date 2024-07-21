      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2162.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2162.CPv  $
      *
      *   Rev 2.2   Sep 19 2016 07:33:28   t13ae31
      *AKI01458: SUH Namensvorwort bei Versicherten Personen OZP
      *
      *   Rev 2.1   Feb 27 2008 20:04:56   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 17:31:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2162                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2162                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.09.2016 / 10:13:46 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2162
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2162.
           05  PI-DRKI2162-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2162-UVPPUFVPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2162-UVPVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2162-UVPVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2162.
           05  PO-DRKI2162-RC                     PIC  X(003).
               88 PO-DRKI2162-RC-OK               VALUE 'OK '.
               88 PO-DRKI2162-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

