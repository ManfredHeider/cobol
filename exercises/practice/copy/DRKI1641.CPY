      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1641.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1641.CPv  $
      *
      *   Rev 2.1   Apr 07 2004 14:21:40   t13ex27
      *Persönliche Anrede für mehrfachbetreute Kunden
      *
      *   Rev 2.0   Feb 12 2004 10:33:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1641                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1641                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.02.2004/11:17:41
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00402!13.02.04!2.1 ! Update f. KVBKUNDEDUZENKZ !I.Frenzel *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1641
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1641.
           05  PI-DRKI1641-KEY.


             10 PI-DRKI1641-KVBKIPERSONENID   PIC X(13).
             10 PI-DRKI1641-KVBKIPERSONENROLLE   PIC X(01).
             10 PI-DRKI1641-KVBVBPERSONENID   PIC X(13).
             10 PI-DRKI1641-KVBVBPERSONENROLLE   PIC X(01).


           05  PI-DRKI1641-DATEN.


             10 PI-DRKI1641-KVBKUNDEDUZENKZ   PIC X(01).


      *
       01  PO-DRKI1641.
           05  PO-DRKI1641-RC                 PIC  X(003).
               88 PO-DRKI1641-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1641-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
