      *****************************************************************
      *    COPY-MEMBER: DRAN0380                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0380
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090!28.06.21! NEU                          ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

      ****************************************************************
      *
       01  PI-DRAN0380.
           05  PI-DRAN0380-KEY.

             10 PI-DRAN0380-CABA-HVANTNUM       PIC  X(0009).

           05  PI-DRAN0380-DATEN.

             10 PI-DRAN0380-CABA-KENN-UPD       PIC  X(0008).
             10 PI-DRAN0380-CABA-PIN            PIC S9(9) COMP.
      *
       01  PO-DRAN0380.
           05  PO-DRAN0380-RC                   PIC  X(003).
               88 PO-DRAN0380-RC-OK             VALUE 'OK '.
               88 PO-DRAN0380-RC-NOT-FOUND      VALUE 'NFD'.          

      *****************************************************************
      *---------------------- ENDE DRAN0380 --------------------------* 
      *****************************************************************
