      *****************************************************************
      *    COPY-MEMBER: DRKI1303                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1303                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  06.11.2001/17:09:42
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1303
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1303.
           05  PI-DRKI1303-KEY.


             10 PI-DRKI1303-BENMITAROLLENID   PIC X(13).
             10 PI-DRKI1303-BENMITAROLLENKZ   PIC X(01).
             

           05  PI-DRKI1303-DATEN.


             10 PI-DRKI1303-BENVWKENNWORT   PIC X(15).
             

      *
       01  PO-DRKI1303.
           05  PO-DRKI1303-RC                 PIC  X(003).
               88 PO-DRKI1303-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1303-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
