      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2985.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2985.CPv  $
      *
      *   Rev 2.8   Mar 12 2015 09:11:22   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.7   Mar 11 2015 08:56:26   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.6   Mar 04 2015 15:40:54   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.5   Feb 26 2015 21:08:16   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.4   Feb 26 2015 19:59:08   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.3   Feb 26 2015 15:57:16   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.2   Feb 26 2015 15:54:34   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.1   Feb 24 2015 14:46:04   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.0   Feb 24 2015 09:50:50   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2985                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2985                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.03.2015 / 08:45:47 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2985
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2985.
           05  PI-DRKI2985-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2985-KVBKIPERSONENID   PIC  X(0013).
             10 PI-DRKI2985-KVBVBPERSONENID   PIC  X(0013).
             10 PI-DRKI2985-KVBVBPERSONENROLLE   PIC  X(0001).
END   *
 
      *
       01  PO-DRKI2985.
           05  PO-DRKI2985-RC                     PIC  X(003).
               88 PO-DRKI2985-RC-OK               VALUE 'OK '.
               88 PO-DRKI2985-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

