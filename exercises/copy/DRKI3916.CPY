      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3916                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3916                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3916
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3916.
           05  PI-DRKI3916-KEY.


             10 PI-DRKI3916-LTSVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI3916.
           05  PO-DRKI3916-RC                 PIC  X(003).
               88 PO-DRKI3916-RC-OK                        VALUE 'OK '.
               88 PO-DRKI3916-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI3916-DATEN.


             10 PO-DRKI3916-SUM-KLB-AUSS   PIC S9(11)V9(2) COMP-3.
             

      *
      ******* ENDE COPY-MEMBER **************************************