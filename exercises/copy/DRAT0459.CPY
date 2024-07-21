      *****************************************************************
      *    COPY-MEMBER: DRAT0459                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0459                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.02.2009/07:43:17
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
      *    FÜR DIE FUNKTION DRAT0459
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0459.
           05  PI-DRAT0459-KEY.


             10 PI-DRAT0459-DTADATEINUMMER   PIC S9(4) COMP.
             10 PI-DRAT0459-DTAGRUPPE   PIC S9(4) COMP.
             10 PI-DRAT0459-DTALFN   PIC S9(9) COMP.
             10 PI-DRAT0459-DTAMANDANTNR   PIC S9(9) COMP.
             10 PI-DRAT0459-DTAPARTGESID   PIC S9(4) COMP.
             

           05  PI-DRAT0459-DATEN.


             10 PI-DRAT0459-DTAGDGSTATUS   PIC X(1).
             

      *
       01  PO-DRAT0459.
           05  PO-DRAT0459-RC                 PIC  X(003).
               88 PO-DRAT0459-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0459-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
